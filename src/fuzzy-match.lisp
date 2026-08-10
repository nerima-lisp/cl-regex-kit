(in-package #:cl-regex-kit)

(defconstant +default-fuzzy-state-limit+ 1000000
  "Default upper bound on explored fuzzy NFA states for one candidate start.")

(defstruct (fuzzy-state
            (:constructor make-fuzzy-state (pc position cost slots)))
  pc
  position
  cost
  slots)

(defun fuzzy-input-code-at (text position limit byte-mode-p unicode-p)
  "Return the input code point and exclusive end for one fuzzy input unit.

Valid UTF-8 is treated as one Unicode scalar when the instruction requests
Unicode semantics. An invalid or range-truncated sequence is one raw octet;
that keeps approximate byte matching useful while exact matching retains its
strict UTF-8 behavior."
  (if (and byte-mode-p unicode-p)
      (multiple-value-bind (character end valid-p)
          (utf8-character-at text position)
        (if (and valid-p (<= end limit))
            (values (char-code character) end)
          (values (aref text position) (1+ position))))
    (let ((element (aref text position)))
      (values (if (characterp element) (char-code element) element)
              (1+ position)))))

(defun fuzzy-input-end (instruction text position limit byte-mode-p)
  "Return the end of the input unit consumed by INSTRUCTION at POSITION."
  (when (< position limit)
    (let ((unicode-p (instruction-unicode-p instruction)))
      (multiple-value-bind (code first-end)
          (fuzzy-input-code-at text position limit byte-mode-p unicode-p)
        (if (and (eq (inst-op instruction) :line-break)
                 (= code #x0d)
                 (< first-end limit))
            (multiple-value-bind (next-code next-end)
                (fuzzy-input-code-at text first-end limit byte-mode-p unicode-p)
              (if (= next-code #x0a) next-end first-end))
          first-end)))))

(defun fuzzy-input-newline-p (instruction text position end byte-mode-p)
  "Return true when a fuzzy input unit contains the forbidden LF newline."
  (let ((unicode-p (instruction-unicode-p instruction)))
    (multiple-value-bind (code first-end)
        (fuzzy-input-code-at text position end byte-mode-p unicode-p)
      (or
       (= code #x0a)
       (and (eq (inst-op instruction) :line-break)
            (= code #x0d)
            (< first-end end)
            (multiple-value-bind (next-code next-end)
                (fuzzy-input-code-at text first-end end byte-mode-p unicode-p)
              (declare (ignore next-end))
              (= next-code #x0a)))))))

(defun fuzzy-state-key (pc position cost slots)
  "Build a value key that preserves capture-sensitive fuzzy states."
  (list pc position cost (coerce slots 'list)))

(defun fuzzy-result-better-p (candidate current)
  "Return true when CANDIDATE wins the fuzzy result ordering."
  (or
   (null current)
   (< (match-edit-distance candidate) (match-edit-distance current))
   (and
    (= (match-edit-distance candidate) (match-edit-distance current))
    (< (match-result-end candidate) (match-result-end current)))))

(defun fuzzy-match-at-position (regex text start limit max-edits state-limit)
  "Run the regular NFA with bounded edit operations at one START position."
  (let* ((program (regex-program regex))
         (slot-count (slot-count-for-program program))
         (byte-mode-p (byte-regex-p regex))
         (never-newline-p (regex-never-newline-p regex))
         (input-length (length text))
         (seen (make-hash-table :test #'equal))
         (pending nil)
         (states-used 0)
         (best-result nil))
    (labels
        ((enqueue (pc position cost slots)
           (when (and (<= 0 cost max-edits) (<= position limit))
             (let ((key (fuzzy-state-key pc position cost slots)))
               (unless (gethash key seen)
                 (when (>= states-used state-limit)
                   (error 'fuzzy-match-limit-error
                          :kind :states
                          :limit state-limit
                          :used states-used))
                 (setf (gethash key seen) t)
                 (incf states-used)
                 (push (make-fuzzy-state pc position cost slots) pending))))))
      (enqueue 0 start 0 (make-array slot-count :initial-element nil))
      (loop while pending
            do (let* ((state (pop pending))
                      (pc (fuzzy-state-pc state))
                      (position (fuzzy-state-position state))
                      (cost (fuzzy-state-cost state))
                      (slots (fuzzy-state-slots state))
                      (instruction (aref program pc)))
                 (case (inst-op instruction)
                   (:split
                    ;; Push B first so A retains the compiled branch priority.
                    (enqueue (inst-b instruction) position cost slots)
                    (enqueue (inst-a instruction) position cost slots))
                   (:jmp
                    (enqueue (inst-a instruction) position cost slots))
                   (:save
                    (let ((saved-slots (copy-seq slots)))
                      (setf (aref saved-slots (inst-a instruction)) position)
                      (enqueue (inst-b instruction) position cost saved-slots)))
                   ((:bol :eol :bos :eos :boundary :non-boundary
                     :word-start :word-end :word-start-half :word-end-half)
                    (when (zero-width-instruction-matches-p
                           instruction text position input-length byte-mode-p)
                      (enqueue (inst-a instruction) position cost slots)))
                   ((:char :class :any :line-break)
                    (let ((input-end
                            (fuzzy-input-end
                             instruction text position limit byte-mode-p)))
                      (when input-end
                        (multiple-value-bind (exact-end exact-p)
                            (instruction-match-end
                             instruction text position limit byte-mode-p
                             never-newline-p)
                          (when exact-p
                            (enqueue (inst-b instruction) exact-end cost slots))
                          (when (and (< cost max-edits)
                                     (not (and never-newline-p
                                               (fuzzy-input-newline-p
                                                instruction text position input-end
                                                byte-mode-p))))
                            ;; A substitution is unnecessary when the exact
                            ;; edge already reaches the same target at lower
                            ;; cost, but an insertion remains useful there.
                            (unless exact-p
                              (enqueue (inst-b instruction) input-end (1+ cost) slots))
                            ;; Insertion consumes input without advancing the
                            ;; regex program.
                            (enqueue pc input-end (1+ cost) slots))))
                      ;; Deletion skips one regex atom without consuming input.
                      (when (< cost max-edits)
                        (enqueue (inst-b instruction) position (1+ cost) slots))))
                   (:match
                    (let ((candidate
                            (make-match-result-from-slots
                             slots slot-count nil cost)))
                      (when (fuzzy-result-better-p candidate best-result)
                        (setf best-result candidate))))
                   (otherwise
                    (error "Fuzzy matching encountered unsupported NFA instruction ~S"
                           (inst-op instruction))))))
      best-result)))

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

(defun fuzzy-scan-at (regex text start &key end timeout (max-edits 1)
                                          (state-limit +default-fuzzy-state-limit+))
  "Find REGEX's leftmost minimum-edit match in TEXT at or after START."
  (fuzzy-scan regex text
              :start start
              :end end
              :timeout timeout
              :max-edits max-edits
              :state-limit state-limit))

(defun fuzzy-search (regex text &key (max-edits 1) (start 0) end timeout
                                   (state-limit +default-fuzzy-state-limit+))
  "Discoverable search-named alias for FUZZY-SCAN."
  (fuzzy-scan regex text
              :start start
              :end end
              :timeout timeout
              :max-edits max-edits
              :state-limit state-limit))

(defun fuzzy-search-at (regex text start &key end timeout (max-edits 1)
                                           (state-limit +default-fuzzy-state-limit+))
  "Discoverable search-named alias for FUZZY-SCAN-AT."
  (fuzzy-scan-at regex text start
                 :end end
                 :timeout timeout
                 :max-edits max-edits
                 :state-limit state-limit))

(defun fuzzy-match (pattern text &key (max-edits 1) (start 0) end timeout
                                   (state-limit +default-fuzzy-state-limit+))
  "Compile character PATTERN and return its bounded fuzzy match in TEXT."
  (fuzzy-scan
   (if (regex-p pattern) pattern (compile-regex pattern))
   text
   :start start
   :end end
   :timeout timeout
   :max-edits max-edits
   :state-limit state-limit))

(defun byte-fuzzy-match (pattern text &key (max-edits 1) (start 0) end timeout
                                         (state-limit +default-fuzzy-state-limit+))
  "Compile byte PATTERN and return its bounded fuzzy match in octet TEXT."
  (fuzzy-scan
   (if (and (regex-p pattern) (byte-regex-p pattern))
       pattern
       (compile-byte-regex pattern))
   text
   :start start
   :end end
   :timeout timeout
   :max-edits max-edits
   :state-limit state-limit))
