(in-package #:cl-regex-kit)

(defparameter +named-lookaround-verb-aliases+
  '((:lookahead "POSITIVE_LOOKAHEAD" "PLA"
     "NON_ATOMIC_POSITIVE_LOOKAHEAD" "NAPLA")
    (:negative-lookahead "NEGATIVE_LOOKAHEAD" "NLA")
    (:lookbehind "POSITIVE_LOOKBEHIND" "PLB"
     "NON_ATOMIC_POSITIVE_LOOKBEHIND" "NAPLB")
    (:negative-lookbehind "NEGATIVE_LOOKBEHIND" "NLB"))
  "Data-only mapping from control-verb spellings to lookaround kinds.")

(defparameter +non-atomic-lookaround-verb-aliases+
  '("NON_ATOMIC_POSITIVE_LOOKAHEAD"
    "NAPLA"
    "NON_ATOMIC_POSITIVE_LOOKBEHIND"
    "NAPLB")
  "Named lookaround control verbs that request the non-atomic executor.")

(defparameter +control-verb-aliases+
  '((:mark "")
    (:fail "FAIL" "F")
    (:skip "SKIP" "S")
    (:prune "PRUNE" "P")
    (:commit "COMMIT" "C")
    (:then "THEN")
    (:accept "ACCEPT" "A")
    (:mark "MARK"))
  "Data-only mapping from (*VERB...) spellings to AST node verb keywords.")

(defparameter +control-verbs-with-argument+
  '(:mark :skip)
  "Control verbs that accept a :tag argument.")

(defparameter +control-verbs-requiring-argument+
  '(:mark)
  "Control verbs that require a non-empty :tag argument.")

(defun alias-table-lookup (name table)
  (loop for entry in table
        for key = (first entry)
        for aliases = (rest entry)
        when (member name aliases :test #'string=)
          do (return key)))

(defun normalize-control-verb-name (verb-name)
  (string-left-trim "*" verb-name))

(defun named-lookaround-kind (verb-name)
  (alias-table-lookup (normalize-control-verb-name verb-name)
                      +named-lookaround-verb-aliases+))

(defun non-atomic-lookaround-verb-p (verb-name)
  (member (normalize-control-verb-name verb-name)
          +non-atomic-lookaround-verb-aliases+
          :test #'string=))

(defun control-verb-kind (verb-name)
  (or (alias-table-lookup (normalize-control-verb-name verb-name)
                          +control-verb-aliases+)
      (fail "Unknown control verb")))

(defun control-verb-accepts-argument-p (verb)
  (member verb +control-verbs-with-argument+ :test #'eq))

(defun control-verb-requires-argument-p (verb)
  (member verb +control-verbs-requiring-argument+ :test #'eq))

(defmacro dispatch-on-character (value &body clauses)
  (let ((value-var (gensym "CHARACTER-")))
    `(let ((,value-var ,value))
       (cond
         ,@(mapcar
            (lambda (clause)
              (destructuring-bind (characters &body body) clause
                `((or
                    ,@(mapcar
                       (lambda (character)
                         `(char= ,value-var ,character))
                       characters))
                  ,@body)))
            clauses)))))

(defun current-source-position ()
  "The character offset the next token starts at, or the pattern's length at
end of stream -- PARSE-REGEX's token-stream analogue of *REGEX-POSITION*,
used only to locate FAIL's caret when no more specific offset applies."
  (if (< *regex-token-position* (length *regex-tokens*)) (token-start (aref *regex-tokens* *regex-token-position*))
    *regex-length*))

(defun fail (reason &optional (at (current-source-position)))
  (error 'regex-syntax-error :pattern *regex-pattern* :position at :reason reason))

(defun regex-whitespace-token-p (token)
  (and
    (eq (token-type token) :char)
    (sb-unicode:whitespace-p (token-value token))))

(defun regex-comment-start-token-p (token)
  (and (eq (token-type token) :char) (char= (token-value token) #\#)))

(defun regex-newline-token-p (token)
  (and (eq (token-type token) :char) (char= (token-value token) #\Newline)))

(defun skip-extended-trivia (position)
  "Advance POSITION past whitespace and `#`-comments when the live extended
flag is set -- the token-stream analogue of SKIP-EXTENDED-SYNTAX."
  (if (logtest +flag-extended+ *regex-flags*) (let ((tokens *regex-tokens*)
          (length (length *regex-tokens*)))
      (loop (loop while (and (< position length) (regex-whitespace-token-p (aref tokens position)))
              do (incf position)) (if (and (< position length) (regex-comment-start-token-p (aref tokens position))) (progn
            (loop while (and (< position length) (not (regex-newline-token-p (aref tokens position))))
                  do (incf position))
            (when (< position length)
              (incf position)))
          (return position)))) position))

(defun at-end-p ()
  (>= (skip-extended-trivia *regex-token-position*) (length *regex-tokens*)))

(defun peek-token ()
  "The next significant token, skipping extended-mode trivia, or NIL at end
of stream. Does not advance *REGEX-TOKEN-POSITION*."
  (setf *regex-token-position* (skip-extended-trivia *regex-token-position*))
  (and
    (< *regex-token-position* (length *regex-tokens*))
    (aref *regex-tokens* *regex-token-position*)))

(defun take-token ()
  (let ((token (peek-token)))
    (if token (prog1
        token
        (incf *regex-token-position*))
      (fail "Unexpected end of pattern"))))

(defun peek-type (&optional (type nil type-p))
  "With no argument, the next significant token's type, or NIL at end of
stream. With TYPE, whether the next significant token has that type."
  (let ((token (peek-token)))
    (if type-p (and token (eq (token-type token) type))
      (and token (token-type token)))))

(defun peek-value ()
  (let ((token (peek-token)))
    (and token (token-value token))))

(defun flag-p (flag)
  (logtest flag *regex-flags*))

(defun digit-token-p (token)
  (and token
       (eq (token-type token) :char)
       (digit-char-p (token-value token))))

(defun ensure-byte-character (character)
  (when (and
      *regex-byte-mode-p*
      (not (flag-p +flag-unicode+))
      (> (char-code character) #xff))
    (fail "Byte patterns only accept code points in the range 0..255"))
  character)

(defun ensure-byte-class-character (character raw-octet-p)
  (when (and
      *regex-byte-mode-p*
      (not (flag-p +flag-unicode+))
      (not raw-octet-p)
      (> (char-code character) #x7f))
    (fail "Unicode is not allowed in a non-Unicode byte character class"))
  (ensure-byte-character character))

(defun make-literal (character &key raw-octet-p)
  (make-instance
    'literal-node
    :char
    character
    :raw-octet-p
    raw-octet-p
    :case-insensitive-p
    (flag-p +flag-case-insensitive+)
    :unicode-p
    (flag-p +flag-unicode+)))

(defun byte-matcher-p (matcher)
  (case (first matcher)
    (:ranges
      (every
        (lambda (range)
          (<= (cdr range) #xff))
        (second matcher)))
    (:property nil)
    (:negate (byte-matcher-p (second matcher)))
    ((:union :intersection :difference :symmetric-difference)
      (every #'byte-matcher-p (rest matcher)))
    (otherwise nil)))

(defun make-class (ranges &optional negated-p matcher)
  (when (and
      *regex-byte-mode-p*
      (not (flag-p +flag-unicode+))
      matcher
      (not (byte-matcher-p matcher)))
    (fail "Unicode character classes are not available in byte patterns"))
  (make-instance
    'char-class-node
    :ranges
    ranges
    :matcher
    matcher
    :negated-p
    negated-p
    :case-insensitive-p
    (flag-p +flag-case-insensitive+)
    :unicode-p
    (flag-p +flag-unicode+)))

(defun make-concat (children)
  (cond
    ((null children) (make-instance 'concat-node :children nil))
    ((null (cdr children)) (car children))
    (t (make-instance 'concat-node :children children))))
