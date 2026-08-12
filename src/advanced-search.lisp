(in-package #:cl-regex-kit)

(declaim (ftype function validate-text-range %advanced-node-evaluate))

(defun %advanced-result-from-state (state start group-count group-names)
  (let* ((slots (copy-seq (advanced-state-slots state)))
         (slot-count (* 2 (1+ group-count)))
         (reported-start (or (advanced-state-reported-start state) start))
         (mark (advanced-state-mark state))
         (mark-name (if (consp mark) (car mark) mark)))
    (setf (aref slots 0) reported-start
          (aref slots 1) (advanced-state-position state))
    (let ((result
           (make-match-result-from-slots
            slots
            slot-count
            mark-name)))
      (setf (match-result-group-names result) (copy-tree group-names))
      result)))

(defun %advanced-result-end (state)
  (advanced-state-position state))

(defun %advanced-select-result (states shortest-p longest-p)
  (if (or shortest-p longest-p)
      (reduce (lambda (best candidate)
                (if (or (null best)
                        (if shortest-p
                            (< (%advanced-result-end candidate)
                               (%advanced-result-end best))
                            (> (%advanced-result-end candidate)
                               (%advanced-result-end best))))
                    candidate
                    best))
              states
              :initial-value nil)
      (first states)))

(defun %advanced-evaluate-candidate (ast
                                     candidate
                                     context
                                     highest-capture
                                     group-names
                                     shortest-p
                                     longest-p)
  "Evaluate AST at CANDIDATE and return result, next position, and stop flag."
  (let* ((slots
          (%advanced-initialize-capture-stacks
           (make-array
            (1+ (* 2 (1+ highest-capture)))
            :initial-element nil)
           highest-capture))
         (seed
          (%make-advanced-state
           candidate
           slots
           nil
           nil
           nil
           candidate
           0
           nil))
         (states
          (if (or shortest-p longest-p)
              (%advanced-node-evaluate ast seed context 0)
              (%advanced-root-first-results ast seed context 0)))
         (matches nil)
         (skip-to nil)
         (stop-candidate-p nil))
    (block evaluate-candidate
      (dolist (state states)
        (case (advanced-state-control state)
          ((nil :accept) (push state matches))
          (:skip
           (unless skip-to
             (setf skip-to (advanced-state-skip-to state))))
          (:commit
           (let ((committed (%advanced-state-copy state)))
             (setf (advanced-state-control committed) nil
                   (advanced-state-committed-p committed) t)
             (push committed matches)))
          (:commit-failure
           (return-from evaluate-candidate (values nil nil t)))
          ((:prune :then) (setf stop-candidate-p t))))
      (if matches
          (values
           (%advanced-result-from-state
            (%advanced-select-result (nreverse matches) shortest-p longest-p)
            candidate
            highest-capture
            group-names)
           nil
           nil)
          (values nil
                  (if stop-candidate-p
                      (1+ candidate)
                      (max (1+ candidate) (or skip-to (1+ candidate))))
                  nil)))))

(defun %run-advanced-regex (regex
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
    (validate-text-range regex text start end)
    (unless (and (integerp step-limit) (plusp step-limit))
      (error "STEP-LIMIT must be a positive integer"))
    (unless (and (integerp nest-limit) (>= nest-limit 0))
      (error "NEST-LIMIT must be a non-negative integer"))
    (let* ((text-length (length text))
           (limit (or end text-length))
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
             :nest-limit nest-limit
             :callout (regex-callout regex))))
      (loop with candidate = start
            while (<= candidate limit)
            do (multiple-value-bind (result next-candidate stop-p)
                   (%advanced-evaluate-candidate
                    ast candidate context highest-capture group-names shortest-p longest-p)
                 (when (or result stop-p)
                   (return result))
                   (setf candidate next-candidate))))))
