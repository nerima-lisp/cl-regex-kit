;;;; run-coverage.lisp
;;;;
;;;; Compile this checkout with SBCL coverage instrumentation, run the test
;;;; system, and write an HTML report for production source files.
(require :asdf)

(require :sb-cover)

(declaim (optimize sb-cover:store-coverage-data))

(defun script-directory ()
  (make-pathname
    :name
    nil
    :type
    nil
    :defaults
    (or
      *load-truename*
      *compile-file-truename*
      (error "Unable to determine the script location"))))

(defun project-root ()
  (uiop:ensure-directory-pathname
    (or (uiop:getenv "CL_REGEX_KIT_ROOT") (script-directory))))

(defun configure-local-source-registry (root)
  (asdf:initialize-source-registry
    `(:source-registry (:directory ,root) :inherit-configuration)))

(defun coverage-directory ()
  (uiop:ensure-directory-pathname
    (or
      (uiop:getenv "CL_REGEX_KIT_COVERAGE_DIRECTORY")
      (merge-pathnames "coverage/" (script-directory)))))

(defparameter +generated-source-file-names+ (quote
    ("package"
      "unicode-age-data"
      "unicode-age-data-1"
      "unicode-age-data-2"
      "unicode-age-data-3"
      "unicode-binary-property-range-data"
      "unicode-case-folding-data"
      "unicode-extra-binary-property-data"
      "unicode-property-data"
      "unicode-indic-conjunct-break-data"))
  "Declarative files excluded from executable-line coverage reporting.")

(defun source-file-p (source-directory)
  (lambda (file)
    (let ((position (search source-directory file :test #'char-equal)))
      (and
        position
        (zerop position)
        (not
          (member (pathname-name file) +generated-source-file-names+ :test #'string-equal))))))

(let* ((root (project-root))
       (source-directory (namestring (merge-pathnames "src/" root)))
       (output-directory (coverage-directory)))
  (configure-local-source-registry root)
  ;; Force recompilation so stale FASLs cannot bypass SB-COVER instrumentation.
  (asdf:test-system "cl-regex-kit" :force t)
  (sb-cover:report output-directory
                   :if-matches (source-file-p source-directory))
  (format t "~&Coverage report: ~A~%" output-directory)
  (uiop:quit 0))
