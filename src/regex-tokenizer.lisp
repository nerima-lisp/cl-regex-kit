;;;; src/regex-tokenizer.lisp
;;;;
;;;; Turns a pattern string into a (VECTOR CL-PARSER-KIT:TOKEN) that
;;;; regex-grammar.lisp/regex-grammar-classes.lisp parse with cl-parser-kit's
;;;; combinators. A single forward pass, tracking only whether it is
;;;; currently inside a `[...]` character class (CLASS-DEPTH) -- the one
;;;; piece of context regex syntax's lexical rules genuinely depend on: `-`,
;;;; `]`, `^`, POSIX classes, and set operators are meaningful only there, and
;;;; `\b` denotes a backspace character inside a class but a word boundary
;;;; outside one.
;;;;
;;;; Everything else this grammar's context-sensitivity touches -- the live
;;;; Unicode flag toggled by inline `(?u)`/`(?-u)`, byte-mode legality,
;;;; case-insensitivity -- depends on *parser state reached along a
;;;; particular parse path*, not on lexical position, and so cannot be
;;;; resolved in this one-pass, flag-independent tokenizer; every ESCAPE
;;;; token instead carries a fully-decoded but *unvalidated* shape (a plist
;;;; naming its kind and raw payload), leaving flag-dependent legality checks
;;;; and AST construction to the grammar layer, exactly where the live flags
;;;; it needs are in scope.
;;;;
;;;; Two fixed-for-the-whole-parse options, BYTE-MODE and OCTAL, are taken as
;;;; tokenizer arguments (never mutated mid-pattern, unlike the inline
;;;; flags) so their two lexically-decidable legality checks -- `\C` outside
;;;; byte mode, and octal escapes when disabled -- can be raised here, right
;;;; where the offending token is scanned.
;;;;
;;;; Extended (whitespace-insensitive) mode is NOT handled here: since its
;;;; flag is itself one of the live, inline-mutable flags, deciding what to
;;;; skip is a grammar-layer concern -- see SKIP-EXTENDED-TRIVIA in
;;;; regex-grammar.lisp, the token-stream analogue of the original parser's
;;;; SKIP-EXTENDED-SYNTAX. Every whitespace character therefore still
;;;; becomes an ordinary :CHAR token here, unconditionally.
;;;;
;;;; Class set operators (`&&`, `~~`, `--`) are likewise NOT resolved here,
;;;; even though they are two-character and lexically unambiguous: the
;;;; original parser only ever tests for them at true item-boundary
;;;; positions (the top of its union loop), never mid-item, so e.g. the
;;;; second `&` consumed as a range endpoint in `[a-&&b]` is never
;;;; re-examined as the start of a fresh `&&`. A tokenizer that matched `&&`
;;;; wherever the two characters happen to be adjacent would not preserve
;;;; that boundary-only behavior, so set-operator recognition stays in
;;;; regex-grammar-classes.lisp, at the same boundaries the original
;;;; PARSE-UNION/PARSE-EXPRESSION checked them.
(in-package #:cl-regex-kit)

(defun posix-class-lookahead-p (pattern position)
  (and (< (1+ position) (length pattern))
       (char= (char pattern position) #\[)
       (char= (char pattern (1+ position)) #\:)))

(defun scan-posix-class-token (pattern position)
  "POSITION is at the opening `[` of a `[:name:]`/`[:^name:]` construct
already confirmed by POSIX-CLASS-LOOKAHEAD-P. Returns (values (name
. negated-p) next-position)."
  (let* ((scan (+ position 2))
         (negated-p (and (< scan (length pattern)) (char= (char pattern scan) #\^)
                          (prog1 t (incf scan))))
         (name-start scan))
    (loop while (and (< scan (length pattern)) (alpha-char-p (char pattern scan))) do (incf scan))
    (when (= name-start scan)
      (tokenizer-fail pattern name-start "POSIX character class name cannot be empty"))
    (unless (and (< (1+ scan) (length pattern))
                 (char= (char pattern scan) #\:) (char= (char pattern (1+ scan)) #\]))
      (tokenizer-fail pattern name-start "Unclosed POSIX character class"))
    (values (cons (subseq pattern name-start scan) negated-p) (+ scan 2))))

(defun tokenize-escape (pattern position class-mode-p)
  "POSITION is just past the backslash. Returns (values type value
next-position); TYPE is :ESCAPE for every recognized form."
  (let ((escaped (if (< position (length pattern)) (char pattern position)
                      (tokenizer-fail pattern position "Unexpected end of pattern")))
        (next (1+ position)))
    (macrolet ((token (value next-position) `(values :escape ,value ,next-position)))
      (case escaped
        (#\p (multiple-value-bind (name negated-p after) (scan-unicode-property-name pattern next)
               (token (list :kind :unicode-property :name name :negated-p negated-p :from-p nil) after)))
        (#\P (multiple-value-bind (name negated-p after) (scan-unicode-property-name pattern next)
               (token (list :kind :unicode-property :name name :negated-p negated-p :from-p t) after)))
        ((#\d #\D #\w #\W #\s #\S) (token (list :kind :shorthand :which escaped) next))
        (#\b (if class-mode-p
                 (token (list :kind :literal :char (code-char 8)) next)
                 ;; Always the bare form here: a possible `\b{name}` extended
                 ;; form is resolved at grammar time by
                 ;; REGEX-GRAMMAR.LISP's BUILD-ESCAPE-ATOM, which alone can
                 ;; both tolerate live-extended-mode whitespace inside the
                 ;; braces and cleanly express the "no name -- leave `{` for
                 ;; the quantifier grammar" backtrack by simply not
                 ;; consuming the following :LBRACE token, rather than by
                 ;; rewinding a lexical position as the original character-
                 ;; level parser did.
                 (token (list :kind :word-boundary :which :word-boundary) next)))
        ((#\a #\f #\n #\r #\t #\v)
         (multiple-value-bind (character raw-octet-p after) (scan-escaped-character pattern position)
           (declare (ignore raw-octet-p))
           (token (list :kind :control :char character) after)))
        ((#\x #\u #\U)
         ;; A braced body (`\x{...}`, `\u{...}`, `\U{...}`) is scanned by the
         ;; grammar layer instead of here, one significant token at a time
         ;; via REGEX-COLLECT-BRACED-HEX -- unlike the fixed-width forms
         ;; below, its digit count is unbounded, and RE2/Rust regex, like
         ;; the original character-level parser, tolerates extended-mode
         ;; whitespace between the digits; that tolerance depends on the
         ;; *live* extended flag, which this one-pass, flag-independent
         ;; tokenizer cannot evaluate.
         (if (and (< next (length pattern)) (char= (char pattern next) #\{))
             (values :hex-brace-open nil (1+ next))
             (multiple-value-bind (character raw-octet-p after) (scan-escaped-character pattern position)
               (token (list :kind :hex :char character :raw-octet-p raw-octet-p) after))))
        ((#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7)
         (unless *regex-octal-p*
           (tokenizer-fail pattern position "Octal escapes are disabled"))
         (multiple-value-bind (character after) (scan-octal-code pattern next)
           (token (list :kind :octal :char character) after)))
        ((#\8 #\9) (tokenizer-fail pattern position "Backreferences are not supported"))
        (#\A (if class-mode-p (token (list :kind :literal :char escaped) next)
                 (token (list :kind :absolute-start) next)))
        (#\z (if class-mode-p (token (list :kind :literal :char escaped) next)
                 (token (list :kind :absolute-end) next)))
        (#\B (if class-mode-p (token (list :kind :literal :char escaped) next)
                 (token (list :kind :not-word-boundary) next)))
        (#\< (if class-mode-p (token (list :kind :literal :char escaped) next)
                 (token (list :kind :word-start) next)))
        (#\> (if class-mode-p (token (list :kind :literal :char escaped) next)
                 (token (list :kind :word-end) next)))
        (#\C (if class-mode-p (token (list :kind :literal :char escaped) next)
                 (progn
                   (unless *regex-byte-mode-p*
                     (tokenizer-fail pattern position "\\C is only available in byte patterns"))
                   (token (list :kind :any-byte) next))))
        (#\Q (if class-mode-p (token (list :kind :literal :char escaped) next)
                 (multiple-value-bind (text after) (scan-quoted-literal pattern next)
                   (token (list :kind :quoted-literal :text text) after))))
        (otherwise
         (when (ascii-alphanumeric-p escaped)
           (tokenizer-fail pattern position "Invalid escape sequence"))
         (token (list :kind :literal :char escaped) next))))))

(defun structural-token-type (character)
  (case character
    (#\( :lparen) (#\) :rparen) (#\| :pipe) (#\* :star) (#\+ :plus) (#\? :question)
    (#\. :dot) (#\^ :caret) (#\$ :dollar) (#\[ :lbracket) (#\{ :lbrace) (#\} :rbrace)
    (#\, :comma) (otherwise :char)))

(defun tokenize-regex-pattern (pattern)
  "Tokenize PATTERN into a (VECTOR CL-PARSER-KIT:TOKEN), consulting the
fixed-for-this-parse *REGEX-BYTE-MODE-P*/*REGEX-OCTAL-P* dynamic bindings
established by PARSE-REGEX."
  (let ((tokens (make-array 0 :adjustable t :fill-pointer 0))
        (position 0) (length (length pattern)) (class-depth 0))
    (flet ((emit (type value start end)
             (vector-push-extend (make-token :type type :value value :start start :end end) tokens)))
      (loop while (< position length) do
        (let ((start position) (character (char pattern position)))
          (cond
            ((char= character #\\)
             (multiple-value-bind (type value next) (tokenize-escape pattern (1+ position) (plusp class-depth))
               (emit type value start next)
               (setf position next)))
            ((and (plusp class-depth) (posix-class-lookahead-p pattern position))
             (multiple-value-bind (posix next) (scan-posix-class-token pattern position)
               (emit :posix-class posix start next)
               (setf position next)))
            ((plusp class-depth)
             (cond
               ((char= character #\[)
                (incf class-depth)
                (emit :lbracket character start (1+ position))
                (setf position (1+ position)))
               ((char= character #\])
                (decf class-depth)
                (emit :rbracket character start (1+ position))
                (setf position (1+ position)))
               ((char= character #\-)
                (emit :hyphen character start (1+ position))
                (setf position (1+ position)))
               (t
                (emit :char character start (1+ position))
                (setf position (1+ position)))))
            (t
             (let ((type (structural-token-type character)))
               (when (eq type :lbracket) (incf class-depth))
               (emit type character start (1+ position))
               (setf position (1+ position)))))))
      (coerce tokens 'simple-vector))))
