;;;; run-coverage.lisp
;;;;
;;;; Compile this checkout with SBCL coverage instrumentation, run the test
;;;; system, and write HTML plus LCOV reports for production source files.

(require :asdf)
(require :sb-cover)

(declaim (optimize (sb-c:store-coverage-data 3)))

(defun script-directory ()
  (make-pathname :name nil
                 :type nil
                 :defaults (or *load-truename*
                               *compile-file-truename*
                               (error "Unable to determine the script location"))))

(defun project-root ()
  (uiop:ensure-directory-pathname
   (or (uiop:getenv "CL_REGEX_KIT_ROOT")
       (script-directory))))

(defun configure-local-source-registry (root)
  (asdf:initialize-source-registry
   `(:source-registry
     (:tree ,root)
     :inherit-configuration)))

(defun coverage-directory ()
  (uiop:ensure-directory-pathname
   (or (uiop:getenv "CL_REGEX_KIT_COVERAGE_DIRECTORY")
       (merge-pathnames "coverage/" (script-directory)))))

(defun source-file-p (source-directory)
  (lambda (file)
    (let ((position (search source-directory file :test #'char-equal)))
      (and position (zerop position)))))

(let* ((root (project-root))
       (source-directory (namestring (merge-pathnames "src/" root)))
       (output-directory (coverage-directory)))
  (configure-local-source-registry root)
  (asdf:test-system "cl-regex-kit")
  (sb-cover:report output-directory
                   :if-matches (source-file-p source-directory))
  (sb-cover:lcov-report (merge-pathnames "coverage.lcov" output-directory))
  (format t "~&Coverage report: ~A~%" output-directory)
  (uiop:quit 0))
