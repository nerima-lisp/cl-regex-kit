(in-package #:cl-regex-kit)

(declaim (ftype function %advanced-node-evaluate))

(defun %advanced-assertion-success (state outer-position)
  (let ((result (%advanced-state-copy state)))
    (setf (advanced-state-position result) outer-position
          (advanced-state-control result) nil
          (advanced-state-skip-to result) nil)
    result))

(defun %advanced-assertion-successes (results)
  (remove-if-not
   (lambda (candidate)
     (or (%advanced-state-normal-p candidate)
         (eq (advanced-state-control candidate) :accept)))
   results))

(defun %advanced-assertion-output
    (node state successes outer-position)
  (if (assertion-node-negative-p node)
      (unless successes
        (list state))
      (when successes
        (if (assertion-node-non-atomic-p node)
            (mapcar
             (lambda (candidate)
               (%advanced-assertion-success candidate outer-position))
             successes)
            (list
             (%advanced-assertion-success
              (first successes)
              outer-position))))))

(defun %advanced-forward-assertion (node state context depth)
  (let* ((outer-position (advanced-state-position state))
         (results
           (%advanced-node-evaluate
            (assertion-node-child node)
            (%advanced-state-copy state)
            context
            (1+ depth))))
    (%advanced-assertion-output
     node
     state
     (%advanced-assertion-successes results)
     outer-position)))

(defun %advanced-backward-assertion (node state context depth)
  (let* ((outer-position (advanced-state-position state))
         (fixed-length (assertion-node-fixed-length node))
         (starts
           (if (and (integerp fixed-length) (>= fixed-length 0))
               (list (max 0 (- outer-position fixed-length)))
               (loop
                 for position from 0 below (1+ outer-position)
                 collect position)))
         (successes nil))
    (dolist (candidate-start starts)
      (let ((candidate-state (%advanced-state-copy state)))
        (setf (advanced-state-position candidate-state) candidate-start)
        (let* ((results
                 (%advanced-node-evaluate
                  (assertion-node-child node)
                  candidate-state
                  context
                  (1+ depth)))
               (matching
                 (remove-if-not
                  (lambda (candidate)
                    (= (advanced-state-position candidate) outer-position))
                  results)))
          (if (assertion-node-non-atomic-p node)
              (dolist (matching-state matching)
                (push matching-state successes))
              (when matching
                (setf successes (list (first matching)))
                (return))))))
    (when (assertion-node-non-atomic-p node)
      (setf successes (nreverse successes)))
    (%advanced-assertion-output node state successes outer-position)))

(defun %advanced-assertion-result (node state context depth)
  (if (member
       (assertion-node-direction node)
       '(:backward :backwards)
       :test
       #'eq)
      (%advanced-backward-assertion node state context depth)
      (%advanced-forward-assertion node state context depth)))
