;;;; src/regex-grammar.lisp
;;;;
;;;; PARSE-REGEX and the top-level grammar (alternation, concatenation,
;;;; repetition, groups, inline flags, atoms) over the token vector
;;;; TOKENIZE-REGEX-PATTERN produces. Character-class bodies are
;;;; regex-grammar-classes.lisp's concern.
;;;;
;;;; Shared parser state -- position in the token stream, accumulated flags,
;;;; capture bookkeeping -- lives in the dynamically-bound special variables
;;;; DEFVAR'd in parser-syntax.lisp, exactly as it did when this parser
;;;; walked characters directly (see the original rationale, preserved here:
;;;; binding them once in PARSE-REGEX and letting every other function read
;;;; and mutate them directly is the same technique CL-PPCRE's recursive-
;;;; descent parser uses). Swapping the character scanner for
;;;; CL-PARSER-KIT's token/span model changes what *REGEX-TOKEN-POSITION*
;;;; indexes and moves every escape/hex/octal/Unicode-property/POSIX-class
;;;; scan into the tokenizer, but does not change this shape: this grammar's
;;;; alternation/concatenation/repetition tiers have no genuine backtracking
;;;; ambiguity (every branch point resolves on one token of lookahead), so
;;;; they stay hand-written recursive descent over the token vector rather
;;;; than combinator pipelines -- CL-PARSER-KIT's own tokenizer/pratt/
;;;; combinator layers are a poor fit for a context-sensitive, single-
;;;; lookahead, error-position-precise grammar like this one, per its own
;;;; documented design center (token-stream languages with real operator
;;;; precedence). What CL-PARSER-KIT contributes here is its TOKEN/SPAN data
;;;; model and the tokenizer built on it.
(in-package #:cl-regex-kit)

(defun parse-number ()
  (let ((beginning (current-source-position))
        (digits nil))
    (unless (and (peek-token) (digit-token-p (peek-token)))
      (fail "Expected a decimal number"))
    (loop while (and (peek-token) (digit-token-p (peek-token)))
          do (push (token-value (take-token)) digits))
    (let ((count (parse-integer (coerce (nreverse digits) 'string))))
      (when (> count +maximum-repeat-count+)
        (fail "Repetition count exceeds 1000" beginning))
      count)))

(defun read-group-name ()
  (let ((beginning (current-source-position))
        (characters nil))
    (unless (and
        (peek-token)
        (not (eq (peek-type) :escape))
        (capture-name-start-p (peek-value)))
      (fail "Capture name must start with an alphabetic character or underscore"))
    (loop while (and
        (peek-token)
        (not (eq (peek-type) :escape))
        (capture-name-character-p (peek-value)))
          do (push (token-value (take-token)) characters))
    (let ((name (coerce (nreverse characters) 'string)))
      (unless (peek-type :char)
        (fail "Unclosed capture name"))
      (unless (char= (peek-value) #\>)
        (fail "Unclosed capture name"))
      (take-token)
      (when (member name *regex-group-names* :test #'string=)
        (fail "Duplicate capture name" beginning))
      (push name *regex-group-names*)
      name)))

(defun parse-flags ()
  (let ((enabled-p t)
        (seen-p nil)
        (disabled-p nil)
        (seen-flags nil))
    (loop while (and
        (peek-type :char)
        (or
          (member (peek-value) '(#\i #\m #\s #\R #\U #\x #\u))
          (char= (peek-value) #\-)))
          do (if (char= (peek-value) #\-) (progn
          (when disabled-p
            (fail "Inline flags may contain only one -"))
          (take-token)
          (setf enabled-p nil
                disabled-p t))
        (let ((character (token-value (take-token))))
          (when (member character seen-flags)
            (fail "Duplicate inline flag"))
          (push character seen-flags)
          (setf *regex-flags* (update-parser-flag *regex-flags* character enabled-p))
          (setf seen-p t))))
    (unless seen-p
      (fail "Expected inline flags"))))

(defun parse-group ()
  (take-token)
  (with-parser-nesting
    (*regex-nesting-depth*
      *regex-nest-limit*
      (fail "Regular expression exceeds the configured nesting limit"))
    (block parse-group
      (let ((capture-index nil)
            (name nil)
            (capturing-p (not *regex-never-capture-p*))
            (scoped-flags-p nil))
        (when (peek-type :question)
          (take-token)
          (if (and (peek-type :char) (member (peek-value) '(#\: #\< #\P))) (case (peek-value)
              (#\:
                (take-token)
                (setf capturing-p nil))
              (#\<
                (take-token)
                (setf name (read-group-name)
                      capturing-p t))
              (#\P
                (take-token)
                (unless (and (peek-type :char) (char= (peek-value) #\<))
                  (fail "Expected < after ?P"))
                (take-token)
                (setf name (read-group-name)
                      capturing-p t)))
            (let ((saved-flags *regex-flags*))
              (parse-flags)
              (cond
                ((peek-type :rparen)
                  (take-token)
                  (return-from parse-group (make-concat nil)))
                ((and (peek-type :char) (char= (peek-value) #\:))
                  (take-token)
                  (setf capturing-p nil
                        scoped-flags-p saved-flags))
                (t (fail "Expected : or ) after inline flags"))))))
        (when capturing-p
          (setf capture-index (incf *regex-group-count*)))
        (let ((child (parse-alternation)))
          (unless (peek-type :rparen)
            (fail "Unclosed group"))
          (take-token)
          (when scoped-flags-p
            (setf *regex-flags* scoped-flags-p))
          (make-instance 'group-node :child child :capture-index capture-index :name name))))))

(defun parse-atom ()
  (when (at-end-p)
    (fail "Expected an expression"))
  (let ((token (peek-token)))
    (case (token-type token)
      (:lparen (parse-group))
      (:lbracket (parse-class))
      (:dot
        (take-token)
        (make-instance
          'any-char-node
          :dotall-p
          (flag-p +flag-dotall+)
          :crlf-p
          (flag-p +flag-crlf+)
          :line-terminator
          *regex-line-terminator*
          :unicode-p
          (flag-p +flag-unicode+)))
      (:caret
        (take-token)
        (make-instance
          'anchor-node
          :kind
          :start
          :multiline-p
          (flag-p +flag-multiline+)
          :crlf-p
          (flag-p +flag-crlf+)
          :line-terminator
          *regex-line-terminator*))
      (:dollar
        (take-token)
        (make-instance
          'anchor-node
          :kind
          :end
          :multiline-p
          (flag-p +flag-multiline+)
          :crlf-p
          (flag-p +flag-crlf+)
          :line-terminator
          *regex-line-terminator*))
      (:escape
        (take-token)
        (build-escape-atom token))
      (:hex-brace-open
        (take-token)
        (make-literal (collect-braced-hex)))
      ((:star :plus :question :rbrace :pipe :rparen)
        (fail "Unexpected metacharacter"))
      (otherwise
        (take-token)
        (make-literal (token-value token))))))

(defun word-anchor (kind)
  (make-instance 'anchor-node :kind kind :unicode-p (flag-p +flag-unicode+)))

(defun closing-brace-p (token)
  "Whether TOKEN is a `}` -- :RBRACE outside a character class, or a plain
:CHAR with that value inside one, where `}` has no structural meaning of its
own. COLLECT-BRACED-HEX runs in both contexts (`\\x{...}` is valid inside
`[...]` too), so it tests the character rather than the token type."
  (and token (char= (token-value token) #\})))

(defun parse-word-boundary-suffix ()
  "Resolve the optional `{name}` suffix on a bare `\\b` word-boundary escape,
using the same extended-mode-aware significant-token cursor as
COLLECT-BRACED-HEX. Unlike a braced hex/Unicode-property escape, an empty
name is not an error: `\\b{2}` is a repeated bare word-boundary, so a `{`
with no alphabetic-or-hyphen name behind it must be left completely
unconsumed for the quantifier grammar to see -- simply not taking the
:LBRACE token accomplishes exactly that, with no position to rewind."
  (if (peek-type :lbrace) (let ((saved *regex-token-position*)
          (name-characters nil))
      (take-token)
      (loop while (and
          (peek-type :char)
          (or (ascii-alphabetic-p (peek-value)) (char= (peek-value) #\-)))
            do (push (token-value (take-token)) name-characters))
      (if name-characters (let ((name (coerce (nreverse name-characters) 'string)))
          (unless (closing-brace-p (peek-token))
            (fail "Unclosed word boundary"))
          (take-token)
          (cond
            ((string= name "start") :word-start)
            ((string= name "end") :word-end)
            ((string= name "start-half") :word-start-half)
            ((string= name "end-half") :word-end-half)
            (t (fail "Unknown word boundary")))) (progn
          (setf *regex-token-position* saved)
          :word-boundary))) :word-boundary))

(defun collect-braced-hex ()
  "Collect the hex digits of a `\\x{...}`/`\\u{...}`/`\\U{...}` escape after
its HEX-BRACE-OPEN token, using the same extended-mode-aware significant-
token cursor as everywhere else in this grammar -- unlike the tokenizer,
which cannot evaluate the live extended flag, this runs at grammar time, so
`(?x)\\x{ 6 1 }` collects \"61\" exactly as the original character-level
parser's PARSE-BRACED-HEX-CODE (built on the same PEEK-based skipping) did."
  (let ((beginning (current-source-position))
        (digits nil))
    (loop while (and (peek-type :char) (digit-char-p (peek-value) 16))
          do (push (token-value (take-token)) digits))
    (unless digits
      (fail "Expected hexadecimal escape digits" beginning))
    (unless (closing-brace-p (peek-token))
      (fail "Unclosed Unicode escape" beginning))
    (take-token)
    (let ((code (parse-integer (coerce (nreverse digits) 'string) :radix 16)))
      (if (and (< code char-code-limit) (not (<= #xD800 code #xDFFF))) (code-char code)
        (fail "Unicode escape is not a Unicode scalar value" beginning)))))

(defun build-escape-atom (token)
  (let ((escape (token-value token)))
    (ecase (getf escape :kind)
      (:absolute-start (make-instance 'anchor-node :kind :absolute-start))
      (:absolute-end (make-instance 'anchor-node :kind :absolute-end))
      (:word-boundary (word-anchor (parse-word-boundary-suffix)))
      (:not-word-boundary (make-instance 'anchor-node :kind :not-word-boundary
                                         :unicode-p (flag-p +flag-unicode+)))
      (:word-start (word-anchor :word-start))
      (:word-end (word-anchor :word-end))
      (:any-byte (make-instance 'any-char-node :dotall-p t :crlf-p nil
                                :line-terminator *regex-line-terminator* :unicode-p nil))
      (:unicode-property
       (when (and *regex-byte-mode-p* (not (flag-p +flag-unicode+)))
         (fail "Unicode properties are not available in byte patterns" (token-start token)))
       ;; The name's validity was already checked unconditionally in
       ;; SCAN-UNICODE-PROPERTY-NAME at tokenize time (unlike the byte-mode
       ;; gate above, that check does not depend on a live, inline-mutable
       ;; flag), so tokenization would already have failed for an unknown
       ;; name -- nothing to re-check here.
       (make-class nil (not (eq (getf escape :from-p) (getf escape :negated-p)))
                   (list :property (getf escape :descriptor))))
      (:quoted-literal (make-concat (map 'list (lambda (c) (make-literal c)) (getf escape :text))))
      (:control (make-literal (getf escape :char)))
      (:hex (make-literal (getf escape :char)
                           :raw-octet-p (and (getf escape :raw-octet-p) (not (flag-p +flag-unicode+)))))
      (:shorthand (let ((which (getf escape :which)))
                    (make-class nil (member which '(#\D #\W #\S)) (shorthand-matcher which))))
      (:octal (make-literal (getf escape :char) :raw-octet-p (not (flag-p +flag-unicode+))))
      (:literal (make-literal (getf escape :char))))))

(defun parse-quantifier (node)
  (let ((minimum nil)
        (maximum nil)
        (quantified-p nil))
    (case (peek-type)
      (:star
        (take-token)
        (setf minimum 0
              maximum nil
              quantified-p t))
      (:plus
        (take-token)
        (setf minimum 1
              maximum nil
              quantified-p t))
      (:question
        (take-token)
        (setf minimum 0
              maximum 1
              quantified-p t))
      (:lbrace
        (take-token)
        (setf minimum (parse-number)
              maximum minimum
              quantified-p t)
        (cond
          ((peek-type :comma)
            (take-token)
            (setf maximum (unless (peek-type :rbrace)
                (parse-number))))
          ((peek-type :rbrace) nil)
          (t (fail "Expected , or } in repetition")))
        (unless (peek-type :rbrace)
          (fail "Unclosed repetition"))
        (take-token)
        (when (and maximum (> minimum maximum))
          (fail "Repetition minimum exceeds maximum"))))
    (if quantified-p (let ((greedy-p (not (flag-p +flag-ungreedy+))))
        (when (peek-type :question)
          (take-token)
          (setf greedy-p (not greedy-p)))
        (when (member (peek-type) '(:star :plus :question :lbrace))
          (fail "Repeated repetition operator"))
        (make-instance
          'repetition-node
          :child
          node
          :min
          minimum
          :max
          maximum
          :greedy-p
          greedy-p))
      node)))

(defun parse-concatenation ()
  (let ((children nil))
    (loop until (or (at-end-p) (member (peek-type) '(:pipe :rparen)))
          do (push (parse-quantifier (parse-atom)) children))
    (make-concat (nreverse children))))

(defun parse-alternation ()
  (let ((branches (list (parse-concatenation))))
    (loop while (peek-type :pipe)
          do (take-token) (push (parse-concatenation) branches))
    (if (cdr branches) (make-instance 'alternation-node :branches (nreverse branches)) (car branches))))

(defun parse-regex (pattern
    &key
    (initial-flags +flag-unicode+)
    byte-mode
    literal
    never-capture
    (octal t)
    (nest-limit +default-nest-limit+)
    (line-terminator #\Newline))
  "Parse PATTERN into a REGEX-NODE tree.
Signals REGEX-SYNTAX-ERROR on malformed input."
  (unless (stringp pattern)
    (error 'regex-syntax-error :pattern pattern :reason "Pattern must be a string"))
  (check-type initial-flags (integer 0 *))
  (check-type byte-mode boolean)
  (check-type literal boolean)
  (check-type never-capture boolean)
  (check-type octal boolean)
  (check-type nest-limit (integer 0 *))
  (check-type line-terminator character)
  (let* ((*regex-pattern* pattern)
         (*regex-length* (length pattern))
         (*regex-byte-mode-p* byte-mode)
         (*regex-octal-p* octal)
         (*regex-tokens* (tokenize-regex-pattern pattern))
         (*regex-token-position* 0)
         (*regex-group-count* 0)
         (*regex-group-names* nil)
         (*regex-flags* initial-flags)
         (*regex-nesting-depth* 0)
         (*regex-never-capture-p* never-capture)
         (*regex-nest-limit* nest-limit)
         (*regex-line-terminator* line-terminator))
    (let ((ast
          (if literal (prog1
              (make-concat
                (loop for character across pattern
                      collect (make-literal character)))
              (setf *regex-token-position* (length *regex-tokens*)))
            (parse-alternation))))
      (unless (at-end-p)
        (fail "Unexpected trailing input"))
      ast)))
