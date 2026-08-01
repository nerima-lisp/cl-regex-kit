(in-package #:cl-regex-kit)

(defstruct vm-thread pc
  slots)

(progn
  (defstruct (pike-vm-closure-workspace
      (:constructor %make-pike-vm-closure-workspace (marks generation))) marks
    generation)
  (defun make-pike-vm-closure-workspace (size)
    (%make-pike-vm-closure-workspace
      (make-array size :element-type (quote fixnum) :initial-element 0)
      0))
  (defun advance-pike-vm-closure-workspace (workspace program-size)
    (let ((marks (pike-vm-closure-workspace-marks workspace)))
      (unless (= (length marks) program-size)
        (error "Closure workspace length does not match PROGRAM"))
      (if (= (pike-vm-closure-workspace-generation workspace) most-positive-fixnum) (progn
          (fill marks 0)
          (setf (pike-vm-closure-workspace-generation workspace) 1))
        (incf (pike-vm-closure-workspace-generation workspace)))
      (values marks (pike-vm-closure-workspace-generation workspace))))
  (defun pike-vm-closure (program text position length byte-mode-p seeds &key workspace)
    "Compute a capture-aware epsilon closure for SEEDS at POSITION."
    (let ((workspace (or workspace (make-pike-vm-closure-workspace (length program))))
          (head (list nil))
          (tail nil))
      (multiple-value-bind (marks generation) (advance-pike-vm-closure-workspace workspace (length program))
        (setf tail head)
        (labels ((enqueue (pc slots)
                   (setf (cdr tail) (list (make-vm-thread :pc pc :slots slots))
                    tail (cdr tail)))
                 (visit (pc slots)
                   (unless (= (aref marks pc) generation)
                (setf (aref marks pc) generation)
                (let ((instruction (aref program pc)))
                  (case (inst-op instruction)
                    (:split
                      (visit (inst-a instruction) slots)
                      (visit (inst-b instruction) slots))
                    (:jmp (visit (inst-a instruction) slots))
                    (:save
                      (let ((next-slots (copy-seq slots)))
                        (setf (aref next-slots (inst-a instruction)) position)
                        (visit (inst-b instruction) next-slots)))
                    ((:bol
                        :eol
                        :bos
                        :eos
                        :boundary
                        :non-boundary
                        :word-start
                        :word-end
                        :word-start-half
                        :word-end-half)
                      (when (zero-width-instruction-matches-p instruction text position length byte-mode-p)
                        (visit (inst-a instruction) slots)))
                    (otherwise (enqueue pc slots)))))))
          (dolist (seed seeds)
            (visit (vm-thread-pc seed) (vm-thread-slots seed)))
          (cdr head)))))
  (defun pike-vm-set-closure (program text position length byte-mode-p seeds &key workspace)
    "Compute an integer-PC epsilon closure for SEEDS at POSITION."
    (let ((workspace (or workspace (make-pike-vm-closure-workspace (length program))))
          (head (list nil))
          (tail nil))
      (multiple-value-bind (marks generation) (advance-pike-vm-closure-workspace workspace (length program))
        (setf tail head)
        (labels ((enqueue (pc)
                   (setf (cdr tail) (list pc)
                    tail (cdr tail)))
                 (visit (pc)
                   (unless (= (aref marks pc) generation)
                (setf (aref marks pc) generation)
                (let ((instruction (aref program pc)))
                  (case (inst-op instruction)
                    (:split
                      (visit (inst-a instruction))
                      (visit (inst-b instruction)))
                    (:jmp (visit (inst-a instruction)))
                    (:save (visit (inst-b instruction)))
                    ((:bol
                        :eol
                        :bos
                        :eos
                        :boundary
                        :non-boundary
                        :word-start
                        :word-end
                        :word-start-half
                        :word-end-half)
                      (when (zero-width-instruction-matches-p instruction text position length byte-mode-p)
                        (visit (inst-a instruction))))
                    (otherwise (enqueue pc)))))))
          (dolist (pc seeds)
            (visit pc))
          (cdr head))))))
