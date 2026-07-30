;;;; src/regex-set.lisp
;;;;
;;;; Multi-pattern matching built directly from compiled REGEX instances.
(in-package #:cl-regex-kit)

(defclass regex-set ()
  ((regexes :initarg :regexes :reader regex-set-regexes)
   (patterns :initarg :patterns :reader regex-set-source-patterns))
  (:documentation
   "An immutable collection of independently compiled regular expressions."))

(defun regex-set-p (object)
  "Return true when OBJECT is a compiled REGEX-SET."
  (typep object 'regex-set))

(defun compile-regex-set (patterns)
  "Compile PATTERNS into a REGEX-SET.

PATTERNS must be a list or vector of pattern strings.  Its order defines the
indexes returned by REGEX-SET-MATCHES."
  (unless (or (listp patterns) (vectorp patterns))
    (error 'type-error :datum patterns :expected-type '(or list vector)))
  (let ((pattern-vector (map 'vector #'identity patterns)))
    (make-instance 'regex-set
                   :patterns pattern-vector
                   :regexes (map 'vector #'compile-regex pattern-vector))))

(defun regex-set-patterns (regex-set)
  "Return a fresh vector containing REGEX-SET's source patterns."
  (check-type regex-set regex-set)
  (copy-seq (regex-set-source-patterns regex-set)))

(defmacro regex-set (&rest patterns)
  "Compile literal PATTERNS once when the containing file is loaded."
  (unless (every #'stringp patterns)
    (error "REGEX-SET requires string literals, got: ~S" patterns))
  `(load-time-value (compile-regex-set ',patterns) t))

(defun regex-set-matches (regex-set text &key (start 0))
  "Return indexes of patterns in REGEX-SET that match TEXT at or after START.

Indexes are returned in source order, including duplicate source patterns."
  (check-type regex-set regex-set)
  (loop for regex across (regex-set-regexes regex-set)
        for index from 0
        when (is-match-p regex text :start start)
          collect index))

(defun regex-set-match-p (regex-set text &key (start 0))
  "Return true when any pattern in REGEX-SET matches TEXT at or after START."
  (check-type regex-set regex-set)
  (loop for regex across (regex-set-regexes regex-set)
        thereis (is-match-p regex text :start start)))
