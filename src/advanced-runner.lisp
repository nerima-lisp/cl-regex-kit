(in-package #:cl-regex-kit)

(declaim (ftype function call-with-timeout))

(defun run-advanced-regex (regex text &key (start 0) end shortest-p longest-p
                                      never-newline-p timeout)
  "Run compiled REGEX with ordered backtracking and return a MATCH-RESULT.

This public entry point accepts the same TIMEOUT contract as the other
matching APIs.  The timeout covers the complete advanced execution, while
the configured step and nest limits remain independent hard limits."
  (call-with-timeout
   timeout
   (lambda ()
     (%run-advanced-regex regex text
                          :start start
                          :end end
                          :shortest-p shortest-p
                          :longest-p longest-p
                          :never-newline-p never-newline-p))))

(export '(run-advanced-regex advanced-regex-limit-error))
