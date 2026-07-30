;;;; cli/main.lisp
(in-package #:cl-regex-kit/cli)

(defun search-stream (regex stream label invert-match-p line-numbers-p count-only-p stdout)
  "Print (or count) the lines of STREAM that REGEX matches, prefixed by LABEL
and a line number when requested. Returns the number of lines printed."
  (loop with matches = 0
        for line = (read-line stream nil nil)
        for line-number from 1
        while line
        for matched-p = (is-match-p regex line)
        when (if invert-match-p (not matched-p) matched-p)
          do (incf matches)
             (unless count-only-p
               (when label (format stdout "~A:" label))
               (when line-numbers-p (format stdout "~D:" line-number))
               (format stdout "~A~%" line))
        finally (return matches)))

(defun search-file (regex file label invert-match-p line-numbers-p count-only-p stdout stderr)
  "As SEARCH-STREAM, opening FILE first. Returns (values matches error-p)."
  (handler-case
      (with-open-file (stream file)
        (values (search-stream regex stream label invert-match-p line-numbers-p count-only-p stdout) nil))
    (file-error (condition)
      (format stderr "~A: ~A~%" file condition)
      (values 0 t))))

(defun run-grep (invocation)
  "MAKE-APP's :HANDLER: compile the pattern once, then search every FILES
positional (or standard input, when none were given), and return a
grep-compatible exit code (0 on at least one match, 1 on none, 2 on error)."
  (let* ((pattern (positional-value invocation :pattern))
         (files (positional-value invocation :files))
         (invert-match-p (option-value invocation :invert-match))
         (count-only-p (option-value invocation :count))
         (line-numbers-p (option-value invocation :line-number))
         (stdout (invocation-stdout invocation))
         (stderr (invocation-stderr invocation))
         (regex (compile-regex pattern :case-insensitive (option-value invocation :ignore-case)))
         (total-matches 0)
         (error-p nil))
    (if files
        (dolist (file files)
          (multiple-value-bind (matches file-error-p)
              (search-file regex file (and (rest files) file)
                           invert-match-p line-numbers-p count-only-p stdout stderr)
            (incf total-matches matches)
            (when file-error-p (setf error-p t))))
        (setf total-matches
              (search-stream regex *standard-input* nil invert-match-p line-numbers-p count-only-p stdout)))
    (when count-only-p (format stdout "~D~%" total-matches))
    (cond (error-p 2) ((zerop total-matches) 1) (t 0))))

(defparameter *app*
  (make-app
   :name "cl-regex-kit-grep"
   :version "0.1.0"
   :summary "Print lines matching a RE2/Rust-compatible regular expression."
   :description "A small grep built directly on cl-regex-kit, for exercising the library as a real command-line tool."
   :global-options
   (list (make-option :key :ignore-case :name "ignore-case" :short #\i :kind :flag
                       :description "Match case-insensitively.")
         (make-option :key :invert-match :name "invert-match" :short #\v :kind :flag
                       :description "Print non-matching lines instead of matching ones.")
         (make-option :key :count :name "count" :short #\c :kind :flag
                       :description "Print only a count of matching lines per input.")
         (make-option :key :line-number :name "line-number" :short #\n :kind :flag
                       :description "Prefix each printed line with its 1-based line number."))
   :positionals
   (list (make-positional :key :pattern :required-p t
                           :description "The RE2/Rust-compatible regular expression to match.")
         (make-positional :key :files :rest-p t
                           :description "Files to search; reads standard input when none are given."))
   :handler #'run-grep))

(defun main ()
  "cl-regex-kit.asd's :ENTRY-POINT for the cl-regex-kit/cli system's
executable. RUN-APP wants ARGV0 as the first element, the same shape as C's
argv, unlike UIOP:COMMAND-LINE-ARGUMENTS, which already excludes it."
  (uiop:quit (run-app *app* :argv (cons "cl-regex-kit-grep" (uiop:command-line-arguments)))))
