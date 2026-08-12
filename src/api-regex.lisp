;;;; src/api-regex.lisp
;;;;
;;;; The compiled regex value object and its metadata accessors.
(in-package #:cl-regex-kit)

(defclass regex ()
  ((program :initarg :program :reader regex-program)
   (ast :initarg :ast :reader regex-ast)
   (advanced-p :initarg :advanced-p :reader regex-advanced-p
               :initform nil)
   (advanced-step-limit :initarg :advanced-step-limit
                        :reader regex-advanced-step-limit)
   (advanced-nest-limit :initarg :advanced-nest-limit
                        :reader regex-advanced-nest-limit)
   (callout :initarg :callout :reader regex-callout
            :initform nil)
   (group-count :initarg :group-count :reader regex-group-count)
   (static-capture-count :initarg :static-capture-count
                         :reader regex-static-capture-count)
   (group-names :initarg :group-names :reader regex-group-names)
   (source :initarg :source :reader regex-source-value)
   (never-newline-p :initarg :never-newline-p :reader regex-never-newline-p
                    :initform nil)
   (byte-mode-p :initarg :byte-mode-p :reader byte-regex-p :initform nil))
  (:documentation "A compiled regular expression with a safe NFA or advanced AST execution path."))

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
