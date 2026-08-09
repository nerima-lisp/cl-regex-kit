(in-package #:cl-regex-kit)

(defun %advanced-evaluate-candidate (ast context candidate capture-count)
  "Evaluate AST from CANDIDATE and return its ordered states."
  (let ((slots
          (%advanced-initialize-capture-stacks
           (make-array
            (1+ (* 2 (1+ capture-count)))
            :initial-element nil)
           capture-count)))
    (%advanced-node-evaluate
     ast
     (%make-advanced-state
      candidate
      slots
      nil
      nil
      nil
      candidate
      0
      nil)
     context
     0)))

(defun %advanced-collect-candidate-results (states context)
  "Collect matches and control signals from one candidate's STATES.

Return MATCHES, the first SKIP-TO position, whether the candidate must stop,
and whether a commit failure terminates the whole search."
  (let ((matches nil)
        (skip-to nil)
        (stop-candidate-p nil))
    (dolist (state states)
      (case (advanced-state-control state)
        ((nil :accept)
         (push state matches))
        (:skip
         (unless skip-to
           (setf skip-to (advanced-state-skip-to state))))
        (:commit
         (let ((committed
                 (let ((*advanced-context* context))
                   (%advanced-state-copy state))))
           (setf (advanced-state-control committed) nil
                 (advanced-state-committed-p committed) t)
           (push committed matches)))
        (:commit-failure
         (return-from %advanced-collect-candidate-results
           (values nil nil nil t)))
        ((:prune :then)
         (setf stop-candidate-p t))))
    (values (nreverse matches) skip-to stop-candidate-p nil)))

(defun run-advanced-regex (regex
                           text
                           &key
                           (start 0)
                           end
                           shortest-p
                           longest-p
                           never-newline-p)
  "Run compiled REGEX with ordered backtracking and return a MATCH-RESULT.

REGEX supplies the AST, capture metadata, resource limits, and byte mode.
AST is evaluated leftmost-first. TEXT may be a string or an octet vector;
byte offsets are preserved for octet input. The configured step and nest
limits are hard limits and signal ADVANCED-REGEX-LIMIT-ERROR when exceeded.
The result uses the same group-zero and (START . END) capture locations as
RUN-PIKE-VM."
  (check-type regex regex)
  (let* ((ast (regex-ast regex))
         (group-count (regex-group-count regex))
         (group-names (regex-group-names regex))
         (step-limit (regex-advanced-step-limit regex))
         (nest-limit (regex-advanced-nest-limit regex))
         (byte-mode-p (byte-regex-p regex)))
    (check-type ast regex-node)
    (check-type never-newline-p boolean)
    (check-type byte-mode-p boolean)
    (when (and shortest-p longest-p)
      (error "SHORTEST-P and LONGEST-P cannot both be true"))
    (let ((limit (validate-text-range regex text start end)))
      (unless (and (integerp step-limit) (> step-limit 0))
        (error "STEP-LIMIT must be a positive integer"))
      (unless (and (integerp nest-limit) (>= nest-limit 0))
        (error "NEST-LIMIT must be a non-negative integer"))
      (let* ((text-length (length text))
             (highest-capture (max 0 (ast-group-count ast) group-count))
             (context
              (make-advanced-context
               :text text
               :search-start start
               :limit limit
               :text-length text-length
               :byte-mode-p byte-mode-p
               :never-newline-p never-newline-p
               :root ast
               :group-count highest-capture
               :group-names group-names
               :step-limit step-limit
               :steps 0
               :state-limit step-limit
               :state-count 0
               :nest-limit nest-limit
               :callout (regex-callout regex))))
        (loop with candidate = start
              while (<= candidate limit)
              do (multiple-value-bind
                     (matches skip-to stop-candidate-p failure-p)
                     (%advanced-collect-candidate-results
                      (%advanced-evaluate-candidate
                       ast
                       context
                       candidate
                       highest-capture)
                      context)
                   (when failure-p
                     (return-from run-advanced-regex nil))
                   (when matches
                     (return-from
                      run-advanced-regex
                      (%advanced-result-from-state
                       (%advanced-select-result
                        matches
                        shortest-p
                        longest-p)
                       candidate
                       highest-capture
                       group-names)))
                   (setf candidate
                         (if stop-candidate-p
                             (1+ candidate)
                             (max
                              (1+ candidate)
                              (or skip-to (1+ candidate)))))))))))
