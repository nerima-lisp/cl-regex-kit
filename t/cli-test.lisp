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

(defun render-grep-count-output (mode files counts)
  (ecase mode
    (labelled
     (with-output-to-string (stream)
       (loop for file in files
             for count in counts
             do (format stream "~A:~D~%" file count))))
    ((single-file stdin)
     (format nil "~D~%" (first counts)))))

(it-grep-count-cases
  "prints count-only results per file and labels every input"
  (labelled
   ("match
miss
match
" "miss
match
")
   ("-c" "match")
   ""
   0
   (2 1))
  (single-file
   ("match
miss
")
   ("-c" "match")
   ""
   0
   (1))
  (stdin
   nil
   ("-c" "match")
   "miss
miss
"
   1
   (0)))

(it-grep-error-cases
  "reports CLI errors with stable status codes and diagnostics"
  (invalid-regex ("(") 2 "" "invalid regular expression:")
  (standard-input ("match") 2 "" "standard input:"))
