(in-package #:cl-regex-kit)

(defun run-pike-vm-set (program
    pattern-count
    text
    &key
    (start 0)
    end
    stop-at-first-match-p
    never-newline-p
    (matches nil matches-supplied-p))
  "Run merged PROGRAM once and return matching pattern indexes.\n\nWhen STOP-AT-FIRST-MATCH-P is true, return true as soon as any member matches.\nOtherwise return all matching indexes in source order. PROGRAM is produced by\nMERGE-NFA-PROGRAMS. Set matching carries only integer program counters."
  (declare (type simple-vector program))
  (check-type never-newline-p boolean)
  (when (and
      matches-supplied-p
      (or
        (not (typep matches (quote bit-vector)))
        (/= (length matches) pattern-count)))
    (error
      (quote type-error)
      :datum
      matches
      :expected-type
      `(bit-vector ,pattern-count)))
  (let* ((length (length text))
         (limit (or end length))
         (byte-mode-p (not (stringp text)))
         (workspace (make-pike-vm-closure-workspace (length program)))
         (match-bits
        (cond
          (stop-at-first-match-p nil)
          (matches-supplied-p matches)
          (t (make-array pattern-count :element-type (quote bit) :initial-element 0)))))
    (unless (and (integerp start) (integerp limit) (<= 0 start limit length))
      (error "START and END must define a range within TEXT"))
    (when (and matches-supplied-p (not stop-at-first-match-p))
      (fill match-bits 0))
    (flet ((closure (seeds position)
             (pike-vm-set-closure
            program
            text
            position
            length
            byte-mode-p
            seeds
            :workspace
            workspace)))
      (when (plusp pattern-count)
        (let ((pending (make-array (1+ (- limit start)) :initial-element nil)))
          (loop for position from start to limit
                do (let* ((pending-index (- position start))
                   (seeds
                  (prog1
                    (nreverse (aref pending pending-index))
                    (setf (aref pending pending-index) nil)))
                   (current (closure (nconc seeds (list 0)) position)))
              (dolist (pc current)
                (let ((instruction (aref program pc)))
                  (case (inst-op instruction)
                    (:set-match
                      (if stop-at-first-match-p (return-from run-pike-vm-set t)
                        (setf (aref match-bits (inst-a instruction)) 1)))
                    ((:char :class :any :line-break)
                      (multiple-value-bind (next-position matched-p) (instruction-match-end
                          instruction
                          text
                          position
                          limit
                          byte-mode-p
                          never-newline-p)
                        (when matched-p
                          (push (inst-b instruction) (aref pending (- next-position start)))))))))))))
      (cond
        (stop-at-first-match-p nil)
        (matches-supplied-p match-bits)
        (t
          (loop for index below pattern-count
                when (= 1 (aref match-bits index))
                  collect index))))))
