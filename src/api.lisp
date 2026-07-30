;;;; src/api.lisp
;;;;
;;;; The public entry points: compile a pattern once, then scan/match it
;;;; against text and read the result back out.
(in-package #:cl-regex-kit)

(defclass regex ()
  ((program :initarg :program :reader regex-program)
   (group-count :initarg :group-count :reader regex-group-count)
   (static-capture-count :initarg :static-capture-count
                         :reader regex-static-capture-count)
   (group-names :initarg :group-names :reader regex-group-names)
   (source :initarg :source :reader regex-source-value)
   (never-newline-p :initarg :never-newline-p :reader regex-never-newline-p
                    :initform nil)
   (byte-mode-p :initarg :byte-mode-p :reader byte-regex-p :initform nil))
   (:documentation "A pattern compiled to a Thompson-NFA program, ready to match."))

(defun regex-p (object)
  "Return true when OBJECT is a compiled REGEX."
  (typep object 'regex))

(defun regex-source (regex)
  "Return a fresh copy of REGEX's source pattern."
  (check-type regex regex)
  (copy-seq (regex-source-value regex)))

(defun regex-capture-count (regex)
  "Return the total number of capture groups, including the overall match."
  (check-type regex regex)
  (1+ (regex-group-count regex)))

(defun escape-character-p (character)
  "Return true when CHARACTER must be quoted in a generated pattern."
  (find character "\\\\.^$|?*+()[]{}#&-~"))

(defun escape (string)
  "Quote STRING so it matches literally when passed to COMPILE-REGEX.

This follows Rust regex-syntax's conservative meta-character quoting."
  (check-type string string)
  (with-output-to-string (output)
    (loop for character across string do
      (when (escape-character-p character)
        (write-char #\\ output))
      (write-char character output))))

(defun validate-literal-compiler-options (macro-name options)
  "Signal an error unless OPTIONS is a compile-time keyword argument list.

Shared by every literal-compiling macro (REGEX, BYTE-REGEX, REGEX-SET,
BYTE-REGEX-SET) so each one states only its own MACRO-NAME and pattern shape."
  (unless (evenp (length options))
    (error "~A options must be a keyword argument list, got: ~S" macro-name options))
  (loop for (key value) on options by #'cddr do
    (unless (keywordp key)
      (error "~A options must use keyword names, got: ~S" macro-name key))
    (unless (constantp value)
      (error "~A options must be compile-time constants, got: ~S" macro-name value))))

(defmacro regex (pattern &rest options)
  "Compile the literal PATTERN once when the containing file is loaded.

OPTIONS are passed to COMPILE-REGEX."
  (unless (stringp pattern)
    (error "REGEX requires a string literal, got: ~S" pattern))
  (validate-literal-compiler-options "REGEX" options)
  `(load-time-value (compile-regex ,pattern ,@options) t))

(defmacro byte-regex (pattern &rest options)
  "Compile literal PATTERN once as a byte-oriented regular expression.

OPTIONS are passed to COMPILE-BYTE-REGEX."
  (unless (stringp pattern)
    (error "BYTE-REGEX requires a string literal, got: ~S" pattern))
  (validate-literal-compiler-options "BYTE-REGEX" options)
  `(load-time-value (compile-byte-regex ,pattern ,@options) t))

(defun collect-group-names (node)
  "Return an alist mapping capture names in NODE to their numeric indexes."
  (typecase node
    (group-node
     (append (when (group-node-name node)
               (list (cons (group-node-name node) (group-node-capture-index node))))
             (collect-group-names (group-node-child node))))
    (concat-node (mapcan #'collect-group-names (concat-node-children node)))
    (alternation-node (mapcan #'collect-group-names (alternation-node-branches node)))
    (repetition-node (collect-group-names (repetition-node-child node)))
    (otherwise nil)))

(defun matcher-contains-unicode-property-p (matcher)
  "Return true when MATCHER depends on Unicode scalar matching."
  (case (first matcher)
    (:property t)
    (:ranges nil)
    (otherwise
     (some #'matcher-contains-unicode-property-p (rest matcher)))))

(defun raw-class-can-match-invalid-utf8-p (node)
  "Return true when raw NODE can consume an octet invalid in UTF-8."
  (or (and (char-class-node-matcher node)
           (matcher-contains-unicode-property-p (char-class-node-matcher node)))
      (some (lambda (range) (> (cdr range) #x7f))
            (char-class-node-ranges node))
      (loop for octet from #x80 to #xff
            thereis (class-matches-octet-p node octet))))

(defun ast-can-match-invalid-utf8-p (node)
  "Return true when NODE admits invalid UTF-8 through a raw scope."
  (typecase node
    (literal-node
     (and (not (literal-node-unicode-p node))
          (literal-node-raw-octet-p node)
          (> (char-code (literal-node-char node)) #x7f)))
    (char-class-node
     (and (not (char-class-node-unicode-p node))
          (raw-class-can-match-invalid-utf8-p node)))
    (any-char-node (not (any-char-node-unicode-p node)))
    (concat-node
     (some #'ast-can-match-invalid-utf8-p (concat-node-children node)))
    (alternation-node
     (some #'ast-can-match-invalid-utf8-p (alternation-node-branches node)))
    (repetition-node
     (ast-can-match-invalid-utf8-p (repetition-node-child node)))
    (group-node
     (ast-can-match-invalid-utf8-p (group-node-child node)))
    (otherwise nil)))

(defun validate-regex-compile-options
    (byte-mode-p &key case-insensitive multi-line dot-matches-new-line
                   swap-greed ignore-whitespace (unicode t) crlf literal
                   never-capture never-newline (octal t)
                   (line-terminator #\Newline)
                   (size-limit +maximum-instruction-count+)
                   (nest-limit +default-nest-limit+))
  "Validate the keyword options shared by regex and regex-set compilation.

Returns (values LINE-TERMINATOR FLAGS): the normalized line terminator and
the parser flag bitmask MAKE-PARSER-FLAGS derives from the boolean options,
computed here once so COMPILE-REGEX/COMPILE-BYTE-REGEX pass PARSE-REGEX the
exact flags this validation already checked, rather than a second,
independently-computed call that could silently drift from it."
  (check-type byte-mode-p boolean)
  (let ((flags (make-parser-flags
                :case-insensitive case-insensitive
                :multi-line multi-line
                :dot-matches-new-line dot-matches-new-line
                :swap-greed swap-greed
                :ignore-whitespace ignore-whitespace
                :unicode unicode
                :crlf crlf)))
    (check-type literal boolean)
    (check-type never-capture boolean)
    (check-type never-newline boolean)
    (check-type octal boolean)
    (check-type nest-limit (integer 0 *))
    (check-type size-limit (integer 1 *))
    (values
     (cond
       ((characterp line-terminator)
        (when (> (char-code line-terminator) #x7f)
          (error 'type-error :datum line-terminator :expected-type 'character))
        line-terminator)
       ((and byte-mode-p (typep line-terminator '(integer 0 255)))
        (code-char line-terminator))
       (t
        (error 'type-error
               :datum line-terminator
               :expected-type (if byte-mode-p
                                  '(or character (integer 0 255))
                                  'character))))
     flags)))

(defun finish-compiled-regex (ast pattern byte-mode-p size-limit never-newline)
  "Shared tail of COMPILE-REGEX/COMPILE-BYTE-REGEX: compile AST to an NFA
program and wrap it as a REGEX instance."
  (multiple-value-bind (program group-count)
      (compile-to-nfa ast pattern :instruction-limit size-limit)
    (multiple-value-bind (static-group-count static-p)
        (ast-static-capture-count ast)
      (make-instance 'regex :program program :group-count group-count
                     :static-capture-count (and static-p (1+ static-group-count))
                     :group-names (collect-group-names ast) :source (copy-seq pattern)
                     :never-newline-p never-newline :byte-mode-p byte-mode-p))))

(defun %compile-pattern (pattern byte-mode-p case-insensitive multi-line dot-matches-new-line
                          swap-greed ignore-whitespace unicode crlf literal never-capture
                          never-newline octal line-terminator size-limit nest-limit)
  "Shared body of COMPILE-REGEX and COMPILE-BYTE-REGEX, which differ only in
BYTE-MODE-P: whether PARSE-REGEX runs in byte mode, whether the resulting AST
is UTF-8-normalized, and whether the invalid-UTF-8-admission check applies
(byte regexes are meant to admit arbitrary octets; character regexes, which
match Lisp strings, cannot represent an invalid one)."
  (multiple-value-bind (line-terminator flags)
      (validate-regex-compile-options
       byte-mode-p
       :case-insensitive case-insensitive :multi-line multi-line
       :dot-matches-new-line dot-matches-new-line :swap-greed swap-greed
       :ignore-whitespace ignore-whitespace :unicode unicode :crlf crlf
       :literal literal :never-capture never-capture :never-newline never-newline
       :octal octal :line-terminator line-terminator :size-limit size-limit
       :nest-limit nest-limit)
    (let ((ast (parse-regex pattern :initial-flags flags :nest-limit nest-limit
                             :byte-mode byte-mode-p :literal literal
                             :never-capture never-capture :octal octal
                             :line-terminator line-terminator)))
      (if byte-mode-p
          (setf ast (normalize-byte-literals ast))
          (when (ast-can-match-invalid-utf8-p ast)
            (error 'regex-syntax-error
                   :pattern pattern
                   :reason "Character regexes cannot match invalid UTF-8 bytes")))
      (finish-compiled-regex ast pattern byte-mode-p size-limit never-newline))))

(defun compile-regex (pattern &key case-insensitive multi-line
                                dot-matches-new-line swap-greed
                                ignore-whitespace (unicode t) crlf
                                literal never-capture never-newline
                                (octal t)
                                (line-terminator #\Newline)
                                (size-limit +maximum-instruction-count+)
                                (nest-limit +default-nest-limit+))
  "Compile PATTERN for string matching with RE2/Rust-compatible options."
  (%compile-pattern pattern nil case-insensitive multi-line dot-matches-new-line
                     swap-greed ignore-whitespace unicode crlf literal never-capture
                     never-newline octal line-terminator size-limit nest-limit))

(defun compile-byte-regex (pattern &key case-insensitive multi-line
                                     dot-matches-new-line swap-greed
                                     ignore-whitespace (unicode t) crlf
                                     literal never-capture never-newline
                                     (octal t)
                                     (line-terminator #\Newline)
                                     (size-limit +maximum-instruction-count+)
                                     (nest-limit +default-nest-limit+))
  "Compile PATTERN for octet-vector matching with RE2/Rust-compatible options."
  (%compile-pattern pattern t case-insensitive multi-line dot-matches-new-line
                     swap-greed ignore-whitespace unicode crlf literal never-capture
                     never-newline octal line-terminator size-limit nest-limit))

(defun regex-capture-names (regex)
  "Return a fresh vector of capture names indexed by capture group."
  (check-type regex regex)
  (let ((names (make-array (regex-capture-count regex) :initial-element nil)))
    (dolist (entry (regex-group-names regex) names)
      (setf (aref names (cdr entry)) (car entry)))))

(defun regex-group-index (regex name)
  "Return the numeric capture index for NAME in REGEX, or NIL when unknown."
  (check-type regex regex)
  (check-type name string)
  (cdr (assoc name (regex-group-names regex) :test #'string=)))

(deftype octet-vector () '(array (unsigned-byte 8) (*)))
