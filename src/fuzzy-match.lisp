(in-package #:cl-regex-kit)

(defun fuzzy-scan (regex text &key (max-edits 1) (start 0) end timeout
                                  (state-limit +default-fuzzy-state-limit+))
  "Find the leftmost minimum-edit match of regular REGEX in TEXT.

MAX-EDITS bounds Levenshtein-style insertions, deletions, and substitutions.
At a given start position the result minimizes edit distance, then chooses the
earliest end. STATE-LIMIT bounds explored NFA states per candidate start and
signals FUZZY-MATCH-LIMIT-ERROR when exceeded. Advanced regexes are rejected
explicitly because their ordered-backtracking semantics are not approximated.
The returned MATCH-RESULT reports the selected distance through
MATCH-EDIT-DISTANCE."
  (check-type regex regex)
  (check-type max-edits (integer 0 *))
  (check-type state-limit (integer 1 *))
  (when (regex-advanced-p regex)
    (error 'fuzzy-match-unsupported
           :pattern (regex-source regex)
           :reason "only regular NFA regexes support bounded fuzzy matching"))
  (let ((result
          (call-with-validated-match
           regex text start end timeout
           (lambda (limit)
             (if (zerop max-edits)
                 (run-pike-vm
                  (regex-program regex) text
                  :start start
                  :end limit
                  :never-newline-p (regex-never-newline-p regex))
               (loop for position from start to limit
                     for candidate =
                       (fuzzy-match-at-position
                        regex text position limit max-edits state-limit)
                     when candidate return candidate))))))
    (when result
      (setf (match-result-group-names result) (regex-group-names regex)))
    result))

(define-forwarding-wrapper fuzzy-scan-at
    (regex text start &key end timeout (max-edits 1)
                      (state-limit +default-fuzzy-state-limit+))
  "Find REGEX's leftmost minimum-edit match in TEXT at or after START."
  fuzzy-scan
  (regex text
         :start start
         :end end
         :timeout timeout
         :max-edits max-edits
         :state-limit state-limit))

(define-forwarding-wrapper fuzzy-match
    (pattern text &key (max-edits 1) (start 0) end timeout
                   (state-limit +default-fuzzy-state-limit+))
  "Compile character PATTERN and return its bounded fuzzy match in TEXT."
  fuzzy-scan
  ((if (regex-p pattern) pattern (compile-regex pattern))
   text
   :start start
   :end end
   :timeout timeout
   :max-edits max-edits
   :state-limit state-limit))

(define-forwarding-wrapper byte-fuzzy-match
    (pattern text &key (max-edits 1) (start 0) end timeout
                     (state-limit +default-fuzzy-state-limit+))
  "Compile byte PATTERN and return its bounded fuzzy match in octet TEXT."
  fuzzy-scan
  ((if (and (regex-p pattern) (byte-regex-p pattern))
       pattern
       (compile-byte-regex pattern))
   text
   :start start
   :end end
   :timeout timeout
   :max-edits max-edits
   :state-limit state-limit))
