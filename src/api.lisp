;;;; src/api.lisp
;;;;
;;;; The public entry points: compile a pattern once, then scan/match it
;;;; against text and read the result back out.
(in-package #:cl-regex-kit)

(defclass regex ()
  ((program :initarg :program :reader regex-program)
   (group-count :initarg :group-count :reader regex-group-count)
   (source :initarg :source :reader regex-source))
  (:documentation "A pattern compiled to a Thompson-NFA program, ready to match."))

(defun compile-regex (pattern)
  "Parse and compile PATTERN (a string) into a REGEX.
Signals REGEX-SYNTAX-ERROR on malformed input."
  (let ((ast (parse-regex pattern)))
    (multiple-value-bind (program group-count) (compile-to-nfa ast)
      (make-instance 'regex :program program :group-count group-count :source pattern))))

(defun scan (regex text &key (start 0))
  "Find the leftmost-first match of REGEX in TEXT at or after START.
Returns a MATCH-RESULT, or NIL if there is no match."
  (run-pike-vm (regex-program regex) text :start start))

(defun match (pattern text &key (start 0))
  "Compile PATTERN and SCAN TEXT once. Prefer COMPILE-REGEX + SCAN when
matching the same pattern repeatedly."
  (scan (compile-regex pattern) text :start start))

(defun all-matches (regex text)
  "Return a list of every non-overlapping MATCH-RESULT of REGEX in TEXT,
left to right."
  (declare (ignore regex text))
  (error "ALL-MATCHES is not yet implemented."))

(defun match-string (match-result text)
  "The substring of TEXT the whole match covers."
  (subseq text (match-result-start match-result) (match-result-end match-result)))

(defun match-group-start (match-result index)
  "The start offset of capture group INDEX, or NIL if it did not participate."
  (car (aref (match-result-groups match-result) index)))

(defun match-group-end (match-result index)
  "The end offset of capture group INDEX, or NIL if it did not participate."
  (cdr (aref (match-result-groups match-result) index)))

(defun match-group-string (match-result index text)
  "The substring TEXT captured by group INDEX, or NIL if it did not participate."
  (let ((start (match-group-start match-result index))
        (end (match-group-end match-result index)))
    (when (and start end)
      (subseq text start end))))
