(in-package #:cl-regex-kit)

(declaim (ftype function %advanced-node-evaluate %advanced-root-first-results))

(defun %advanced-evaluate-concat
    (children state context depth &optional first-match-p)
  (labels ((commit-continuation (candidate)
             (let ((result (%advanced-state-copy candidate)))
               (setf (advanced-state-control result) nil
                     (advanced-state-committed-p result) t)
               result))
           (commit-failure (candidate)
             (let ((result (%advanced-state-copy candidate)))
               (setf (advanced-state-control result) :commit-failure)
               result))
           (walk (remaining current optimize-current-p)
             (if (null remaining)
                 (if (eq (advanced-state-control current) :commit)
                     (list (commit-continuation current))
                     (list current))
                 (let ((results
                         (if (and first-match-p optimize-current-p)
                             (%advanced-root-first-results
                              (car remaining)
                              current
                              context
                              (1+ depth))
                             (%advanced-node-evaluate
                              (car remaining)
                              current
                              context
                              (1+ depth)))))
                   (if (null results)
                       (if (and first-match-p optimize-current-p)
                           (walk remaining current nil)
                           (if (advanced-state-committed-p current)
                               (list (commit-failure current))
                               nil))
                       (let ((reversed-output nil))
                         (dolist (candidate results)
                           (cond
                             ((eq (advanced-state-control candidate) :commit)
                              (let ((continued
                                      (walk
                                       (cdr remaining)
                                       (commit-continuation candidate)
                                       first-match-p)))
                                (cond
                                  ((null continued)
                                   (return-from walk
                                     (list (commit-failure candidate))))
                                  ((some
                                    (lambda (item)
                                      (eq
                                       (advanced-state-control item)
                                       :commit-failure))
                                    continued)
                                   (return-from walk continued))
                                  (t
                                   (dolist (item continued)
                                     (push item reversed-output))))))
                             ((eq
                               (advanced-state-control candidate)
                               :commit-failure)
                              (return-from walk (list candidate)))
                             ((%advanced-state-terminal-p candidate)
                              (push candidate reversed-output))
                             (t
                              (let ((continued
                                      (walk
                                       (cdr remaining)
                                       candidate
                                       first-match-p)))
                                (when
                                    (some
                                     (lambda (item)
                                       (eq
                                        (advanced-state-control item)
                                        :commit-failure))
                                     continued)
                                  (return-from walk continued))
                                (dolist (item continued)
                                  (push item reversed-output))))))
                         (if (and first-match-p
                                  optimize-current-p
                                  (null reversed-output))
                             (walk remaining current nil)
                             (nreverse reversed-output)))))))
    )
    (walk children state first-match-p)))

(defun %advanced-evaluate-alternation
    (branches state context depth &optional stop-on-normal-p)
  (labels ((try-branches (remaining)
             (when remaining
               (let ((results
                       (%advanced-node-evaluate
                        (car remaining)
                        state
                        context
                        (1+ depth))))
                 (cond
                   ((some
                     (lambda (candidate)
                       (eq (advanced-state-control candidate) :then))
                     results)
                    (append
                     (remove-if
                      (lambda (candidate)
                        (eq (advanced-state-control candidate) :then))
                      results)
                     (try-branches (cdr remaining))))
                   ((and stop-on-normal-p (some #'%advanced-state-normal-p results))
                    results)
                   ((some #'%advanced-state-terminal-p results)
                    results)
                   (t
                    (append results (try-branches (cdr remaining)))))))))
    (try-branches branches)))

(defun %advanced-progressing-results (results position)
  (remove-if-not
   (lambda (candidate)
     (and
      (%advanced-state-normal-p candidate)
      (> (advanced-state-position candidate) position)))
   results))

(defun %advanced-repeat-results (node state context depth possessive-p)
  (let ((child
         (if possessive-p (possessive-repetition-node-child node)
             (repetition-node-child node)))
        (minimum
         (if possessive-p (possessive-repetition-node-min node)
             (repetition-node-min node)))
        (maximum
         (if possessive-p (possessive-repetition-node-max node)
             (repetition-node-max node)))
        (greedy-p
         (if possessive-p (possessive-repetition-node-greedy-p node)
             (repetition-node-greedy-p node))))
    (if possessive-p
        (labels ((consume (current count)
                   (%advanced-step context (1+ depth))
                   (if (and maximum (>= count maximum))
                       (and (>= count minimum) (list current))
                       (let* ((results
                                (%advanced-node-evaluate
                                 child
                                 current
                                 context
                                 (1+ depth)))
                              (control
                                (find-if
                                 (lambda (candidate)
                                   (not (%advanced-state-normal-p candidate)))
                                 results))
                              (progress
                                (find-if
                                 (lambda (candidate)
                                   (and
                                    (%advanced-state-normal-p candidate)
                                    (>
                                     (advanced-state-position candidate)
                                     (advanced-state-position current))))
                                 results)))
                         (cond
                           (control (list control))
                           (progress (consume progress (1+ count)))
                           ((>= count minimum) (list current))
                           (t nil))))))
          (consume state 0))
        (labels ((expand (current count)
                   (%advanced-step context (1+ depth))
                   (let ((stop
                           (when (>= count minimum)
                             (list current))))
                     (if (and maximum (>= count maximum))
                         stop
                         (let* ((results
                                  (%advanced-node-evaluate
                                   child
                                   current
                                   context
                                   (1+ depth)))
                                (controls
                                  (remove-if #'%advanced-state-normal-p results))
                                (progress
                                  (%advanced-progressing-results
                                   results
                                   (advanced-state-position current)))
                                (deeper
                                  (mapcan
                                   (lambda (candidate)
                                     (expand candidate (1+ count)))
                                   progress)))
                           (if greedy-p
                               (append deeper controls stop)
                               (append stop deeper controls)))))))
          (expand state 0)))))

(defun %advanced-group-result (node state context depth &optional root-first-p)
  (let* ((index (group-node-capture-index node))
         (balance-name (group-node-balance-name node))
         (balance-index
           (and balance-name
                (%advanced-capture-index-by-name balance-name context))))
    (when (and balance-name (null balance-index))
      (return-from %advanced-group-result))
    (let ((entered (%advanced-state-copy state)))
      (when balance-name
        (let ((stacks (%advanced-capture-stacks entered)))
          (unless (and stacks (aref stacks balance-index))
            (return-from %advanced-group-result))
          (pop (aref stacks balance-index))
          (%advanced-sync-capture-slot entered balance-index)))
      (when index
        (let ((stacks (%advanced-capture-stacks entered)))
          (unless stacks
            (return-from %advanced-group-result))
          (push (list (advanced-state-position state))
                (aref stacks index))
          (%advanced-sync-capture-slot entered index)))
      (mapcar
       (lambda (candidate)
         (let ((result (%advanced-state-copy candidate)))
           (when index
             (let* ((stacks (%advanced-capture-stacks result))
                    (stack (and stacks (aref stacks index)))
                    (top (first stack)))
               (when top
                 (setf (cdr top) (advanced-state-position candidate))
                 (%advanced-sync-capture-slot result index))))
           result))
       (if root-first-p
           (%advanced-root-first-results
            (group-node-child node)
            entered
            context
            (1+ depth))
           (%advanced-node-evaluate
            (group-node-child node)
            entered
            context
            (1+ depth)))))))

(defun %advanced-condition-true-p (condition state context depth)
  (cond
    ((eq condition :define)
     nil)
    ((eq condition :recursion)
     (plusp (or (advanced-state-recursion-depth state) 0)))
    ((and
      (consp condition)
      (member (first condition)
              '(:recursion-index :recursion-name)
              :test #'eq))
     (let ((target (advanced-state-recursion-target state)))
       (and
        (plusp (or (advanced-state-recursion-depth state) 0))
        (typep target 'group-node)
        (case (first condition)
          (:recursion-index
           (and
            (integerp (group-node-capture-index target))
            (= (group-node-capture-index target) (second condition))))
          (:recursion-name
           (and
            (group-node-name target)
            (string= (group-node-name target) (second condition))))))))
    ((and
      (consp condition)
      (member (first condition)
              '(:capture-index :name)
              :test #'eq))
     (let ((indices
             (case (first condition)
               (:capture-index
                (list (second condition)))
               (:name
                (or
                 (%advanced-capture-indices-by-name (second condition) context)
                 (let ((group
                         (%advanced-find-group
                          (advanced-context-root context)
                          :name
                          (second condition))))
                   (and
                    group
                    (list (%advanced-capture-index-for-group group)))))))))
       (some
        (lambda (index)
          (%advanced-capture-participated-p index state))
        indices)))
    ((typep condition 'regex-node)
     (some
      #'%advanced-state-normal-p
      (%advanced-node-evaluate condition state context (1+ depth))))
    (t nil)))

(defun %advanced-optional-node-evaluate (node state context depth)
  (if node
      (%advanced-node-evaluate node state context (1+ depth))
      (list state)))

(defun %advanced-conditional-result (node state context depth)
  (if (%advanced-condition-true-p
       (conditional-node-condition node)
       state
       context
       depth)
      (%advanced-optional-node-evaluate
       (conditional-node-yes-branch node)
       state
       context
       depth)
      (%advanced-optional-node-evaluate
       (conditional-node-no-branch node)
       state
       context
       depth)))

(defun %advanced-atomic-result (node state context depth)
  (let ((results
          (%advanced-node-evaluate
           (atomic-node-child node)
           state
           context
           (1+ depth))))
    (when results
      (list (first results)))))

(defun %advanced-root-first-results (node state context depth)
  "Evaluate transparent nodes with ordered first-match short-circuiting.

The general evaluator must retain every alternative because a following
concatenation node may need to backtrack into one of them. At the root of a
leftmost-first match there is no following node, so an ordinary result from
the first root alternative is already decisive. Keeping this optimization at
the root avoids evaluating an intentionally unbounded recursive fallback such
as `(?:a|(?R))` after `a` has matched, without changing nested backtracking."
  (typecase node
    (alternation-node
     (%advanced-evaluate-alternation
      (alternation-node-branches node)
      state
      context
      depth
      t))
    (group-node
     (%advanced-group-result node state context depth t))
    (concat-node
     (%advanced-evaluate-concat
      (concat-node-children node)
      state
      context
      depth
      t))
    (otherwise
     (%advanced-node-evaluate node state context depth))))
