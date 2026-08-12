(in-package #:cl-regex-kit)

(defstruct match-result start
  end
  groups
  group-names
  mark
  (edit-distance 0))

(defun slot-count-for-program (program)
  (declare (type simple-vector program))
  (loop for instruction across program
        when (eq (inst-op instruction) :save)
          maximize (1+ (inst-a instruction)) into maximum
        finally (return (or maximum 0))))

(defun make-match-result-from-slots (slots slot-count &optional mark (edit-distance 0))
  (let ((groups (make-array (/ slot-count 2) :initial-element nil)))
    (dotimes (index (length groups))
      (let ((from (aref slots (* 2 index)))
            (to (aref slots (1+ (* 2 index)))))
        (when (and from to)
          (setf (aref groups index) (cons from to)))))
    (make-match-result
     :start (aref slots 0)
     :end (aref slots 1)
     :groups groups
     :mark mark
     :edit-distance edit-distance)))

(defun run-pike-vm-boolean (program text &key (start 0) end never-newline-p)
  (declare (type simple-vector program))
  (let* ((length (length text))
         (limit (or end length))
         (byte-mode-p (not (stringp text)))
         (workspace (make-pike-vm-closure-workspace (length program))))
    (unless (and (integerp start) (integerp limit) (<= 0 start limit length))
      (error "START and END must define a range within TEXT"))
    (let ((pending (make-array (1+ (- limit start)) :initial-element nil)))
      (loop for position from start to limit
            do (let* ((pending-index (- position start))
                      (seeds (prog1
                                 (nreverse (aref pending pending-index))
                               (setf (aref pending pending-index) nil)))
                      (current
                        (pike-vm-boolean-closure
                         program text position length byte-mode-p
                         (cons 0 seeds) :workspace workspace)))
                 (dolist (pc current)
                   (let ((instruction (aref program pc)))
                     (case (inst-op instruction)
                       (:match (return-from run-pike-vm-boolean t))
                       ((:char :class :any :line-break)
                        (multiple-value-bind (next-position matched-p)
                            (instruction-match-end instruction text position limit
                                                    byte-mode-p never-newline-p)
                          (when matched-p
                            (push (inst-b instruction)
                                  (aref pending (- next-position start))))))))))))
    nil))

(defun run-pike-vm (program text &key (start 0) end shortest-p longest-p
                                      never-newline-p boolean-p slot-count)
  "Run PROGRAM against TEXT and return its leftmost-first match, if any.

When SHORTEST-P is true, select the earliest ending match at the leftmost
start position instead of applying the usual greedy/lazy branch priority.
When LONGEST-P is true, select the longest match at the leftmost start
position, retaining the usual branch priority to resolve equal-length paths."
  (declare (type simple-vector program))
  (when (and shortest-p longest-p)
    (error "SHORTEST-P and LONGEST-P cannot both be true"))
  (check-type never-newline-p boolean)
  (let* ((slot-count (or slot-count (slot-count-for-program program)))
         (length (length text))
         (limit (or end length))
         (byte-mode-p (not (stringp text)))
         (workspace (unless (and boolean-p (zerop slot-count))
                     (make-pike-vm-closure-workspace (length program)))))
    (when (and boolean-p (zerop slot-count))
      (return-from run-pike-vm
        (run-pike-vm-boolean program text :start start :end end
                             :never-newline-p never-newline-p)))
    (unless (and (integerp start) (integerp limit) (<= 0 start limit length))
      (error "START and END must define a range within TEXT"))
    (flet ((closure (seeds position)
             (pike-vm-closure
            program
            text
            position
            length
            byte-mode-p
            seeds
            :workspace
            workspace)))
      (let ((pending (make-array 5 :initial-element nil))
            (best-result nil))
        (loop for position from start to limit
              do (let* ((pending-index (mod (- position start) 5))
                 (seeds
                (prog1
                  (nreverse (aref pending pending-index))
                  (setf (aref pending pending-index) nil)))
                 (fresh-seed
                (unless (and longest-p best-result)
                  (make-vm-thread :pc 0 :slots (make-array slot-count :initial-element nil))))
                 (current
                (closure
                  (if fresh-seed
                      (if boolean-p
                          (cons fresh-seed seeds)
                        (nconc seeds (list fresh-seed)))
                    seeds)
                  position))
                 (blocking-p nil)
                 (earliest-start
                (and
                  shortest-p
                  (loop for thread in current
                        minimize (aref (vm-thread-slots thread) 0)))))
            (dolist (thread current)
              (let ((instruction (aref program (vm-thread-pc thread))))
                (case (inst-op instruction)
                  (:match
                    (if boolean-p
                        (return-from run-pike-vm t)
                      (let ((slots (vm-thread-slots thread)))
                        (cond
                        (longest-p
                          (when (or
                              (null best-result)
                              (< (aref slots 0) (match-result-start best-result))
                              (and
                                (= (aref slots 0) (match-result-start best-result))
                                (> (aref slots 1) (match-result-end best-result))))
                            (setf best-result (make-match-result-from-slots slots slot-count))))
                        ((if shortest-p (= (aref slots 0) earliest-start)
                            (not blocking-p))
                            (return-from run-pike-vm (make-match-result-from-slots slots slot-count)))))))
                  ((:char :class :any :line-break)
                    (multiple-value-bind (next-position matched-p) (instruction-match-end
                        instruction
                        text
                        position
                        limit
                        byte-mode-p
                        never-newline-p)
                      (when matched-p
                        (setf blocking-p t)
                        (push
                          (make-vm-thread :pc (inst-b instruction) :slots (vm-thread-slots thread))
                          (aref pending (mod (- next-position start) 5))))))))))
              finally (return best-result))))))
