;;;; run-tests.lisp
;;;;
;;;; Test runner script following the nerima-lisp package convention:
;;;; anchor to this checkout, load the dedicated test system, and run it.

(require :asdf)

(setf *default-pathname-defaults*
      (make-pathname :name nil
                     :type nil
                     :version nil
                     :defaults (or *load-truename*
                                   *compile-file-truename*
                                   (error "Unable to determine the script location"))))

(handler-case
    (progn
      (asdf:load-system "cl-regex-kit/test")
      (uiop:symbol-call '#:cl-regex-kit/test '#:run-tests)
      (uiop:quit 0))
  (error (condition)
    (format *error-output* "Test run failed: ~A~%" condition)
    (uiop:quit 1)))
