(progn
  (in-package #:cl-regex-kit/test)
  (defun call-with-temporary-grep-files (contents function)
    (let ((paths
          (loop repeat (length contents)
                collect (merge-pathnames
              (format nil "cl-regex-kit-cli-~A.txt" (gensym))
              (uiop:temporary-directory)))))
      (unwind-protect (progn
          (loop for path in paths
                for content in contents
                do (with-open-file (stream
                path
                :direction
                :output
                :if-exists
                :supersede
                :if-does-not-exist
                :create)
              (write-string content stream)))
          (funcall function paths))
        (dolist (path paths)
          (when (probe-file path)
            (delete-file path))))))
  (defun run-grep-cli (arguments &optional (input ""))
    (let ((stdout (make-string-output-stream))
          (stderr (make-string-output-stream))
          (*standard-input*
          (if (streamp input) input
            (make-string-input-stream input))))
      (let ((exit-code
            (cl-cli:run-app
              cl-regex-kit/cli::*app*
              :argv
              (cons "cl-regex-kit-grep" arguments)
              :stdout
              stdout
              :stderr
              stderr)))
        (values
          exit-code
          (get-output-stream-string stdout)
          (get-output-stream-string stderr)))))
  (it
    "prints count-only results per file and labels every input"
    (call-with-temporary-grep-files
      (list "match
miss
match
" "miss
match
")
      (lambda (paths)
        (let ((files (mapcar (function namestring) paths)))
          (multiple-value-bind (exit-code stdout stderr) (run-grep-cli (append (list "-c" "match") files))
            (expect exit-code :to-equal 0)
            (expect
              stdout
              :to-equal
              (format nil "~A:2~%~A:1~%" (first files) (second files)))
            (expect stderr :to-equal ""))))))
  (it
    "prints an unlabelled count for one file and standard input"
    (call-with-temporary-grep-files
      (list "match
miss
")
      (lambda (paths)
        (multiple-value-bind (exit-code stdout stderr) (run-grep-cli (list "-c" "match" (namestring (first paths))))
          (expect exit-code :to-equal 0)
          (expect stdout :to-equal (format nil "1~%"))
          (expect stderr :to-equal ""))))
    (multiple-value-bind (exit-code stdout stderr) (run-grep-cli (list "-c" "match") "miss
miss
")
      (expect exit-code :to-equal 1)
      (expect stdout :to-equal (format nil "0~%"))
      (expect stderr :to-equal "")))
  (it
    "reports an invalid regular expression and exits with status 2"
    (multiple-value-bind (exit-code stdout stderr) (run-grep-cli (list "("))
      (expect exit-code :to-equal 2)
      (expect stdout :to-equal "")
      (expect (search "invalid regular expression:" stderr) :to-be-truthy)))
  (it
    "reports a standard-input stream error and exits with status 2"
    (let ((input (make-string-input-stream "match\n")))
      (close input)
      (multiple-value-bind (exit-code stdout stderr) (run-grep-cli (list "match") input)
        (expect exit-code :to-equal 2)
        (expect stdout :to-equal "")
        (expect (search "standard input:" stderr) :to-be-truthy)))))
