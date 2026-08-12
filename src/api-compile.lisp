;;;; src/api-compile.lisp
;;;;
;;;; Public regex compilation, literal macros, and compile-time validation.
(in-package #:cl-regex-kit)

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
    (possessive-repetition-node
     (collect-group-names (possessive-repetition-node-child node)))
    (assertion-node (collect-group-names (assertion-node-child node)))
    (atomic-node (collect-group-names (atomic-node-child node)))
    (conditional-node
     (append (when (typep (conditional-node-condition node) 'regex-node)
               (collect-group-names (conditional-node-condition node)))
             (collect-group-names (conditional-node-yes-branch node))
             (collect-group-names (conditional-node-no-branch node))))
    (subroutine-node
     (when (typep (subroutine-node-target node) 'regex-node)
       (collect-group-names (subroutine-node-target node))))
    (otherwise nil)))

(defun validate-advanced-references (ast pattern)
  "Reject advanced references that cannot resolve in AST.

The parser deliberately accepts forward references and several PCRE spelling
variants.  Resolving them here gives every public compilation entry point the
same deterministic failure instead of deferring an unresolved reference to
advanced matching, where it would otherwise look like an ordinary no-match or
runtime error."
  (let* ((group-count (ast-group-count ast))
         (group-names (collect-group-names ast)))
    (labels ((fail (format-control &rest arguments)
               (error 'regex-syntax-error
                      :pattern pattern
                      :reason (apply #'format nil format-control arguments)))
             (name-string (name)
               (and (or (stringp name) (symbolp name))
                    (string name)))
             (validate-name (name kind)
               (let ((normalized (name-string name)))
                 (unless (and normalized
                              (some (lambda (entry)
                                      (string= normalized (string (car entry))))
                                    group-names))
                   (fail "Unresolved ~A reference ~S" kind name))))
             (validate-index (index kind)
               (unless (and (integerp index)
                            (plusp index)
                            (<= index group-count))
                 (fail "Unresolved ~A reference ~S (capture count is ~D)"
                       kind index group-count)))
             (validate-condition (condition)
               (cond
                 ((typep condition 'regex-node)
                  (visit condition))
                 ((member condition '(:define :recursion) :test #'eq)
                  nil)
                 ((consp condition)
                  (case (first condition)
                    (:capture-index
                     (validate-index (second condition) "capture condition"))
                    (:name
                     (validate-name (second condition) "conditional"))
                    (:recursion-index
                     (validate-index (second condition) "recursion condition"))
                    (:recursion-name
                     (validate-name (second condition) "recursion condition"))
                    (otherwise
                     (fail "Unsupported conditional reference ~S" condition))))
                 (t
                  (fail "Unsupported conditional reference ~S" condition))))
             (validate-subroutine (node)
               (let* ((target (subroutine-node-target node))
                      (name (subroutine-node-name node))
                      (capture-index (subroutine-node-capture-index node))
                      (recursive-p (or (typep node 'recursion-node)
                                       (subroutine-node-recursive-p node))))
                 (when name
                   (validate-name name "subroutine"))
                 (when (and capture-index
                            (not (and recursive-p (zerop capture-index))))
                   (validate-index capture-index "subroutine"))
                 (cond
                   ((typep target 'regex-node)
                    (visit target))
                   ((and target (or (stringp target) (symbolp target)))
                    (validate-name target "subroutine"))
                   ((integerp target)
                    (if (and recursive-p (zerop target))
                        nil
                        (validate-index target "subroutine")))
                   ((and recursive-p (null target)) nil)
                   (t
                    (fail "Unresolved subroutine reference ~S" target)))))
             (visit (node)
               (when (typep node 'regex-node)
                 (typecase node
                   (backreference-node
                    (let ((name (backreference-node-name node))
                          (capture-index (backreference-node-capture-index node)))
                      (cond
                        (name (validate-name name "backreference"))
                        (capture-index (validate-index capture-index "backreference"))
                        (t (fail "Backreference has no target")))))
                   (concat-node
                    (mapc #'visit (concat-node-children node)))
                   (alternation-node
                    (mapc #'visit (alternation-node-branches node)))
                   (repetition-node
                    (visit (repetition-node-child node)))
                   (possessive-repetition-node
                    (visit (possessive-repetition-node-child node)))
                   (group-node
                    (visit (group-node-child node)))
                   (assertion-node
                    (visit (assertion-node-child node)))
                   (atomic-node
                    (visit (atomic-node-child node)))
                   (conditional-node
                    (validate-condition (conditional-node-condition node))
                    (visit (conditional-node-yes-branch node))
                    (visit (conditional-node-no-branch node)))
                   (subroutine-node
                    (validate-subroutine node))
                   (otherwise nil)))))
      (visit ast))))

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
    (possessive-repetition-node
     (ast-can-match-invalid-utf8-p (possessive-repetition-node-child node)))
    (group-node
     (ast-can-match-invalid-utf8-p (group-node-child node)))
    (assertion-node
     (ast-can-match-invalid-utf8-p (assertion-node-child node)))
    (atomic-node
     (ast-can-match-invalid-utf8-p (atomic-node-child node)))
    (conditional-node
     (or (ast-can-match-invalid-utf8-p (conditional-node-yes-branch node))
         (ast-can-match-invalid-utf8-p (conditional-node-no-branch node))))
    (subroutine-node
     (and (typep (subroutine-node-target node) 'regex-node)
          (ast-can-match-invalid-utf8-p (subroutine-node-target node))))
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

(defun finish-compiled-regex (ast pattern byte-mode-p size-limit never-newline nest-limit callout)
  "Shared tail of COMPILE-REGEX/COMPILE-BYTE-REGEX.

Safe patterns are compiled to the existing Thompson-NFA program. Patterns
using features that need ordered backtracking retain their AST and are
executed by the bounded advanced matcher instead."
  (validate-advanced-references ast pattern)
  (annotate-lookbehind-lengths ast byte-mode-p)
  (let ((advanced-p (ast-contains-advanced-p ast)))
    (multiple-value-bind (program group-count)
        (if advanced-p
            (values nil (ast-group-count ast))
            (compile-to-nfa ast pattern :instruction-limit size-limit))
      (multiple-value-bind (static-group-count static-p)
          (ast-static-capture-count ast)
        (make-instance (quote regex)
                       :program program
                       :slot-count (if program
                                       (slot-count-for-program program)
                                     0)
                       :ast ast
                       :advanced-p advanced-p
                       :advanced-step-limit size-limit
                       :advanced-nest-limit nest-limit
                       :callout callout
                       :group-count group-count
                       :static-capture-count (and static-p (1+ static-group-count))
                       :group-names (collect-group-names ast)
                       :source (copy-seq pattern)
                       :never-newline-p never-newline
                       :byte-mode-p byte-mode-p)))))

(defun %compile-pattern (pattern byte-mode-p case-insensitive multi-line dot-matches-new-line
                          swap-greed ignore-whitespace unicode crlf literal never-capture
                          never-newline octal line-terminator size-limit nest-limit callout)
  "Shared body of COMPILE-REGEX and COMPILE-BYTE-REGEX, which differ only in
BYTE-MODE-P: whether PARSE-REGEX runs in byte mode, whether the resulting AST
is UTF-8-normalized, and whether the invalid-UTF-8-admission check applies
(byte regexes are meant to admit arbitrary octets; character regexes, which
match Lisp strings, cannot represent an invalid one)."
  (when (and callout (not (functionp callout)))
    (error "CALLOUT must be a function or NIL"))
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
          (setf ast (normalize-byte-literals ast byte-mode-p))
          (when (ast-can-match-invalid-utf8-p ast)
            (error 'regex-syntax-error
                   :pattern pattern
                   :reason "Character regexes cannot match invalid UTF-8 bytes")))
      (finish-compiled-regex ast pattern byte-mode-p size-limit never-newline
                             nest-limit callout))))

(defmacro define-pattern-compiler (name byte-mode-p domain-description)
  "Define NAME as a COMPILE-REGEX-shaped entry point compiling PATTERN for
DOMAIN-DESCRIPTION with RE2/Rust-compatible options, dispatching to
%COMPILE-PATTERN with BYTE-MODE-P fixed to NIL or T.

COMPILE-REGEX and COMPILE-BYTE-REGEX share every keyword argument and
default; the only difference between them is BYTE-MODE-P and their
docstring\s domain. Generating both from one specification keeps the
keyword lambda list as a single source of truth: a new compilation option
only needs adding here, not once per domain."
  `(defun ,name (pattern &key case-insensitive multi-line
                          dot-matches-new-line swap-greed
                          ignore-whitespace (unicode t) crlf
                          literal never-capture never-newline
                          (octal t)
                          (line-terminator #\Newline)
                          (size-limit +maximum-instruction-count+)
                          (nest-limit +default-nest-limit+)
                          (callout nil))
     ,(format nil "Compile PATTERN for ~A with RE2/Rust-compatible options." domain-description)
     (%compile-pattern pattern ,byte-mode-p case-insensitive multi-line dot-matches-new-line
                       swap-greed ignore-whitespace unicode crlf literal never-capture
                       never-newline octal line-terminator size-limit nest-limit callout)))

(define-pattern-compiler compile-regex nil "string matching")
(define-pattern-compiler compile-byte-regex t "octet-vector matching")
