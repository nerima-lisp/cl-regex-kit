;;;; src/conditions.lisp
(in-package #:cl-regex-kit)

(define-condition cl-regex-kit-error (error) ()
  (:documentation "Base condition for every error CL-REGEX-KIT signals."))

(define-condition regex-syntax-error (cl-regex-kit-error)
  ((pattern :initarg :pattern :reader regex-syntax-error-pattern)
   (position :initarg :position :initform nil :reader regex-syntax-error-position)
   (reason :initarg :reason :reader regex-syntax-error-reason))
  (:report
   (lambda (condition stream)
     (format stream "Invalid regular expression ~S~@[ at position ~D~]: ~A"
             (regex-syntax-error-pattern condition)
             (regex-syntax-error-position condition)
             (regex-syntax-error-reason condition))))
  (:documentation "Signalled by PARSE-REGEX when PATTERN cannot be parsed."))

(define-condition regex-timeout (cl-regex-kit-error)
  ((seconds :initarg :seconds :reader regex-timeout-seconds))
  (:report
   (lambda (condition stream)
     (format stream "Regular expression matching exceeded ~,3F seconds"
             (regex-timeout-seconds condition))))
  (:documentation "Signalled when a matching operation exceeds its time limit."))

(define-condition fuzzy-match-unsupported (cl-regex-kit-error)
  ((pattern :initarg :pattern :reader fuzzy-match-unsupported-pattern)
   (reason :initarg :reason :reader fuzzy-match-unsupported-reason))
  (:report
   (lambda (condition stream)
     (format stream "Fuzzy matching cannot execute ~S: ~A"
             (fuzzy-match-unsupported-pattern condition)
             (fuzzy-match-unsupported-reason condition))))
  (:documentation
   "Signalled when FUZZY-SCAN is requested for a regex outside the regular NFA dialect."))

(define-condition fuzzy-match-limit-error (cl-regex-kit-error)
  ((kind :initarg :kind :reader fuzzy-match-limit-kind)
   (limit :initarg :limit :reader fuzzy-match-limit)
   (used :initarg :used :reader fuzzy-match-limit-used))
  (:report
   (lambda (condition stream)
     (format stream "Fuzzy matching exceeded its ~S limit ~D after ~D states"
             (fuzzy-match-limit-kind condition)
             (fuzzy-match-limit condition)
             (fuzzy-match-limit-used condition))))
  (:documentation
   "Signalled when bounded fuzzy matching exceeds its explicit state limit."))
