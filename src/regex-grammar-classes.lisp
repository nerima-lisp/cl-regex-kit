;;;; src/regex-grammar-classes.lisp
;;;;
;;;; Character-class body grammar: `[...]`, POSIX classes, and the set-
;;;; operator expression grammar (`&&`, `~~`, `--`), over the token vector
;;;; regex-tokenizer.lisp produces. Shares PARSE-REGEX's dynamically-bound
;;;; state (regex-grammar.lisp) the same way its core grammar functions do.
(in-package #:cl-regex-kit)

(defun ranges-matcher (ranges) (list :ranges ranges))

(defun union-matcher (parts)
  (cond ((null parts) (ranges-matcher nil))
        ((null (cdr parts)) (car parts))
        (t (cons :union parts))))

(defun node-matcher (node)
  (let ((matcher (or (char-class-node-matcher node) (ranges-matcher (char-class-node-ranges node)))))
    (if (char-class-node-negated-p node) (list :negate matcher) matcher)))

(defun posix-class-matcher (name negated-p position)
  (let ((ranges (ascii-posix-class-ranges (string-downcase name))))
    (unless ranges (fail "Unsupported POSIX character class" position))
    (let ((matcher (ranges-matcher ranges))) (if negated-p (list :negate matcher) matcher))))

(defun range (start end)
  (ensure-byte-character start)
  (ensure-byte-character end)
  (cons (char-code start) (char-code end)))

(defun shorthand-ranges (character)
  (case character
    ((#\d #\D) (list (range #\0 #\9)))
    ((#\w #\W) (list (range #\a #\z) (range #\A #\Z) (range #\0 #\9) (range #\_ #\_)))
    ((#\s #\S) (list (range #\Space #\Space) (range #\Tab #\Tab) (range #\Newline #\Newline)
                      (range #\Return #\Return) (range #\Page #\Page) (range (code-char 11) (code-char 11))))
    (otherwise nil)))

(defun shorthand-matcher (character)
  (if (flag-p +flag-unicode+)
      (case character
        ((#\d #\D) '(:property "Nd"))
        ((#\w #\W) '(:union (:property "Alphabetic") (:property "Mark") (:property "Nd") (:property "Pc")
                     (:ranges ((95 . 95) (#x200c . #x200d)))))
        ((#\s #\S) '(:property "White_Space"))
        (otherwise nil))
      (let ((ranges (shorthand-ranges character))) (and ranges (list :ranges ranges)))))

(defun class-item-from-escape (token)
  (let ((escape (token-value token)))
    (case (getf escape :kind)
      (:unicode-property
       (when (and *regex-byte-mode-p* (not (flag-p +flag-unicode+)))
         (fail "Unicode properties are not available in byte patterns" (token-start token)))
       ;; See BUILD-ESCAPE-ATOM's :UNICODE-PROPERTY case in regex-grammar.lisp:
       ;; the name's validity was already checked, unconditionally, at
       ;; tokenize time.
       (let ((matcher (list :property (getf escape :name))))
         (values nil (if (eq (getf escape :from-p) (getf escape :negated-p)) matcher (list :negate matcher)) nil)))
      (:shorthand
       (let ((which (getf escape :which)))
         (values nil (if (member which '(#\D #\W #\S)) (list :negate (shorthand-matcher which))
                          (shorthand-matcher which))
                 nil)))
      ((:control :hex :octal :literal) (values (getf escape :char) nil
                                                (and (eq (getf escape :kind) :hex) (getf escape :raw-octet-p)
                                                     (not (flag-p +flag-unicode+))))))))

(defun class-item ()
  "Parse one character-class item: a literal, an escaped literal, or an
escaped matcher (Unicode property or shorthand). Returns (values literal
matcher raw-octet-p) -- MATCHER is non-NIL only for the escaped-matcher case,
mirroring the original CLASS-ITEM's contract."
  (let ((token (take-token)))
    (case (token-type token)
      (:escape (class-item-from-escape token))
      (:hex-brace-open (values (collect-braced-hex) nil nil))
      (otherwise (values (token-value token) nil nil)))))

(defun class-set-operator ()
  "Detect `&&`/`~~`/`--` at the current item-boundary position without
consuming input, mirroring the original parser's boundary-only recognition:
`[a-&&b]` never re-examines the range endpoint's `&` as the start of a fresh
operator, because the original only tests for one here, at a genuine
between-items position."
  (let ((first (peek-token)))
    (when (and first (member (token-type first) '(:char :hyphen)) (member (token-value first) '(#\& #\~ #\-)))
      (let ((saved *regex-token-position*) (first-character (token-value first)))
        (take-token)
        (let ((second (peek-token)))
          (prog1
              (and second (member (token-type second) '(:char :hyphen)) (char= (token-value second) first-character)
                   (case first-character (#\& :intersection) (#\~ :symmetric-difference) (#\- :difference)))
            (setf *regex-token-position* saved)))))))

(defun consume-set-operator ()
  (let ((operator (class-set-operator)))
    (when operator (take-token) (take-token))
    operator))

(defun class-range-lookahead ()
  "Whether the token after the current :HYPHEN forms a range end, i.e. is
neither `]` nor another `-` -- probed without consuming the hyphen."
  (let ((saved *regex-token-position*))
    (take-token)
    (prog1 (let ((next (peek-token))) (and next (not (member (token-type next) '(:rbracket :hyphen)))))
      (setf *regex-token-position* saved))))

(defun parse-union ()
  (let ((ranges nil) (matchers nil) (saw-item-p nil))
    (loop
      (when (at-end-p) (fail "Unclosed character class"))
      (when (or (peek-type :rbracket) (class-set-operator)) (return))
      (setf saw-item-p t)
      (cond
        ((peek-type :posix-class)
         (let ((token (take-token)))
           (when ranges (push (ranges-matcher (nreverse ranges)) matchers) (setf ranges nil))
           (push (posix-class-matcher (car (token-value token)) (cdr (token-value token)) (token-start token))
                 matchers)))
        ((peek-type :lbracket)
         (when ranges (push (ranges-matcher (nreverse ranges)) matchers) (setf ranges nil))
         (push (node-matcher (parse-class)) matchers))
        (t
         (multiple-value-bind (literal matcher raw-octet-p) (class-item)
           (cond
             (matcher
              (when ranges (push (ranges-matcher (nreverse ranges)) matchers) (setf ranges nil))
              (push matcher matchers))
             ((and (peek-type :hyphen) (class-range-lookahead))
              (take-token)
              (multiple-value-bind (end end-matcher end-raw-octet-p) (class-item)
                (when end-matcher (fail "Character-class range endpoint must be literal"))
                (ensure-byte-class-character literal raw-octet-p)
                (ensure-byte-class-character end end-raw-octet-p)
                (when (> (char-code literal) (char-code end)) (fail "Character-class range is reversed"))
                (push (range literal end) ranges)))
             (t
              (ensure-byte-class-character literal raw-octet-p)
              (push (range literal literal) ranges)))))))
    (unless saw-item-p (fail "Character-class set operation requires a right operand"))
    (when ranges (push (ranges-matcher (nreverse ranges)) matchers))
    (union-matcher (nreverse matchers))))

(defun parse-expression ()
  (let ((left (parse-union)))
    (loop for operator = (consume-set-operator) while operator
          do (setf left (list operator left (parse-union))))
    left))

(defun parse-class ()
  (take-token)
  (let ((negated-p (when (and (peek-type :char) (char= (peek-value) #\^)) (take-token))))
    (when (at-end-p) (fail "Unclosed character class"))
    (if (peek-type :rbracket)
        (progn (take-token) (make-class nil negated-p nil))
        (let ((matcher (parse-expression)))
          (unless (peek-type :rbracket) (fail "Unclosed character class"))
          (take-token)
          (make-class nil negated-p matcher)))))
