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
