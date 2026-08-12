(in-package #:cl-regex-kit)

(declaim (ftype function %advanced-node-evaluate))
(declaim (ftype function %advanced-extended-pictographic-p %advanced-grapheme-class %advanced-indic-conjunct-break-class %advanced-grapheme-break-p))

(defun %advanced-backreference-result (node state context &optional (depth 0))
  (let* ((index (%advanced-capture-index node context state))
         (slots (advanced-state-slots state))
         (from
          (and index (< (* 2 index) (length slots)) (aref slots (* 2 index))))
         (to
          (and
           index
           (< (1+ (* 2 index)) (length slots))
           (aref slots (1+ (* 2 index))))))
    (when (and from to)
      (let ((source-position from)
            (target-position (advanced-state-position state))
            (case-insensitive-p (backreference-node-case-insensitive-p node))
            (unicode-p (backreference-node-unicode-p node)))
        (block matched
          (loop while (< source-position to)
                do (progn
                     (%advanced-step context depth)
                     (multiple-value-bind (source-element
                                           source-end
                                           source-valid-p) (%advanced-read-element
                                                            context
                                                            source-position
                                                            unicode-p)
                       (multiple-value-bind (target-element
                                             target-end
                                             target-valid-p) (%advanced-read-element
                                                              context
                                                              target-position
                                                              unicode-p)
                         (unless (and
                                  source-valid-p
                                  target-valid-p
                                  (%advanced-element-equal-p
                                   source-element
                                   target-element
                                   case-insensitive-p
                                   unicode-p))
                           (return-from matched))
                         (setf source-position source-end
                               target-position target-end)))))
          (when (= source-position to)
            (list
             (%make-advanced-state
              target-position
              (advanced-state-slots state)
              nil
              nil
              (advanced-state-mark state)
              (advanced-state-reported-start state)
              (advanced-state-recursion-depth state)
              (advanced-state-recursion-target state)
              (advanced-state-committed-p state)))))))))

(defun %advanced-consuming-result (node state context operation)
  (let ((instruction (make-inst :op operation :a node)))
    (multiple-value-bind (end success-p) (instruction-match-end
                                          instruction
                                          (advanced-context-text context)
                                          (advanced-state-position state)
                                          (advanced-context-limit context)
                                          (advanced-context-byte-mode-p context)
                                          (advanced-context-never-newline-p
                                           context))
      (when success-p
        (list
         (%make-advanced-state
          end
          (advanced-state-slots state)
          nil
          nil
          (advanced-state-mark state)
          (advanced-state-reported-start state)
          (advanced-state-recursion-depth state)
          (advanced-state-recursion-target state)
          (advanced-state-committed-p state)))))))
(defun %advanced-anchor-result (node state context)
  (let ((kind (anchor-node-kind node))
        (position (advanced-state-position state)))
    (cond
      ((eq kind :match-start)
       (when (= position (advanced-context-search-start context))
         (list state)))
      ((eq kind :end-before-final-newline)
       (let* ((text (advanced-context-text context))
              (limit (advanced-context-limit context))
              (byte-mode-p (advanced-context-byte-mode-p context)))
         (labels ((newline-p (value)
                    (if byte-mode-p
                        (= value #x0a)
                        (char= value #\Newline)))
                  (carriage-return-p (value)
                    (if byte-mode-p
                        (= value #x0d)
                        (char= value #\Return))))
           (when (= position
                    (cond
                      ((zerop limit) limit)
                      ((not (newline-p (aref text (1- limit)))) limit)
                      ((and (> limit 1)
                            (carriage-return-p (aref text (- limit 2))))
                       (- limit 2))
                      (t (1- limit))))
             (list state)))))
      ((member kind
               '(:grapheme-boundary
                 :word-boundary-unicode
                 :sentence-boundary)
               :test #'eq)
       (when (%advanced-special-boundary-p node context position)
         (list state)))
      (t
       (let* ((bits
               (logior
                (if (anchor-node-multiline-p node) 1
                  0)
                (if (anchor-node-unicode-p node) 2
                  0)
                (if (anchor-node-crlf-p node) 4
                  0)))
              (instruction
               (make-inst
                :op
                (anchor-kind-op kind)
                :b
                bits
                :c
                (anchor-node-line-terminator node))))
         (when (zero-width-instruction-matches-p
                instruction
                (advanced-context-text context)
                position
                (advanced-context-limit context)
                (advanced-context-byte-mode-p context))
           (list state)))))))

(defun %advanced-control-name (verb)
  (string-upcase (string verb)))

(defun %advanced-control-result (node state)
  (let* ((name (%advanced-control-name (control-verb-node-verb node)))
         (argument (control-verb-node-argument node))
         (mark (advanced-state-mark state))
         (mark-state (if (consp mark) mark (list mark)))
         (mark-positions (cdr mark-state))
         (control
          (cond
            ((member name (list "FAIL" "F") :test (function string=)) :fail)
            ((member name (list "ACCEPT" "A") :test (function string=)) :accept)
            ((member name (list "COMMIT" "C") :test (function string=)) :commit)
            ((member name (list "PRUNE" "P") :test (function string=)) :prune)
            ((member name (list "SKIP" "S") :test (function string=)) :skip)
            ((string= name "THEN") :then)
            ((string= name "MARK") :mark)
            (t (error "Unknown advanced regex control verb ~S" name)))))
    (case control
      (:fail nil)
      (:mark
       (list
        (%make-advanced-state
         (advanced-state-position state)
         (advanced-state-slots state)
         nil
         nil
         (cons
          argument
          (acons
           argument
           (advanced-state-position state)
           (remove
            argument
            mark-positions
            :key (function car)
            :test (function equal))))
         (advanced-state-reported-start state)
         (advanced-state-recursion-depth state)
         (advanced-state-recursion-target state)
         (advanced-state-committed-p state))))
      (otherwise
       (let ((skip-to
               (if (eq control :skip)
                   (cond
                     ((null argument) (advanced-state-position state))
                     ((stringp argument)
                      (let ((entry
                              (assoc
                               argument
                               mark-positions
                               :test (function equal))))
                        (and entry (cdr entry))))
                     ((integerp argument) argument)
                     (t nil))
                   (and (integerp argument) argument))))
         (list
          (%make-advanced-state
           (advanced-state-position state)
           (advanced-state-slots state)
           control
           skip-to
           mark
           (advanced-state-reported-start state)
           (advanced-state-recursion-depth state)
           (advanced-state-recursion-target state)
           (or (advanced-state-committed-p state) (eq control :commit)))))))))


(defun %advanced-callout-result (node state context)
  (let ((callout (advanced-context-callout context)))
    (if callout
        (let ((decision
                (funcall callout
                         (callout-node-number node)
                         (callout-node-tag node)
                         (advanced-state-position state)
                         (advanced-context-text context))))
          (cond
            ((or (null decision) (eq decision :continue))
             (list state))
            ((eq decision :fail) nil)
            (t
             (error "Callout callback must return NIL, :CONTINUE, or :FAIL"))))
        (list state))))

(defun %advanced-subroutine-target (node context)
  (let ((target (subroutine-node-target node)))
    (cond
      ((typep target 'regex-node) target)
      ((and (integerp target) (zerop target)) (advanced-context-root context))
      ((integerp target)
       (%advanced-find-group (advanced-context-root context) :index target))
      ((and target (or (stringp target) (symbolp target)))
       (%advanced-find-group (advanced-context-root context) :name target))
      ((typep node 'recursion-node) (advanced-context-root context))
      (t nil))))

(defun %advanced-subroutine-result (node state context depth)
  (let ((target (%advanced-subroutine-target node context)))
    (unless target
      (error
       "Unresolved advanced regex subroutine target ~S"
       (subroutine-node-target node)))
    (let* ((entry-depth (or (advanced-state-recursion-depth state) 0))
           (entry-target (advanced-state-recursion-target state))
           (entry-slots (%advanced-copy-slots (advanced-state-slots state)))
           (target-index
             (and
              (typep target (quote group-node))
              (group-node-capture-index target)))
           (recursive-state (%advanced-state-copy state))
           (recursive-results
             (progn
               (setf (advanced-state-recursion-depth recursive-state)
                     (1+ entry-depth)
                     (advanced-state-recursion-target recursive-state) target)
               (unless (%advanced-recursion-active-p
                        target
                        (advanced-state-position state)
                        context)
                 (let ((previous-stack
                         (advanced-context-recursion-stack context)))
                   (push
                    (cons target (advanced-state-position state))
                    (advanced-context-recursion-stack context))
                   (unwind-protect
                       (%advanced-node-evaluate
                        target
                        recursive-state
                        context
                        (1+ depth))
                     (setf (advanced-context-recursion-stack context)
                           previous-stack)))))))
      (mapcar
       (lambda (candidate)
         (let ((result (%advanced-state-copy candidate)))
           (setf (advanced-state-recursion-depth result) entry-depth
                 (advanced-state-recursion-target result) entry-target)
           (when
               (and
                (integerp target-index)
                (< (1+ (* 2 target-index)) (length entry-slots))
                (aref entry-slots (* 2 target-index)))
             (setf
              (aref
               (advanced-state-slots result)
               (* 2 target-index))
              (aref entry-slots (* 2 target-index))
              (aref
               (advanced-state-slots result)
               (1+ (* 2 target-index)))
              (aref entry-slots (1+ (* 2 target-index)))))
           (let ((result-stacks (%advanced-capture-stacks result))
                 (entry-stacks
                   (and
                    (plusp (length entry-slots))
                    (let ((stacks
                            (aref entry-slots (1- (length entry-slots)))))
                      (and (vectorp stacks) stacks)))))
             (when
                 (and
                  (integerp target-index)
                  result-stacks
                  entry-stacks)
               (setf
                (aref result-stacks target-index)
                (%advanced-copy-capture-stack
                 (aref entry-stacks target-index)))
               (%advanced-sync-capture-slot result target-index)))
           result))
       recursive-results))))

(defun %advanced-basic-node-result (node state context)
  (typecase node
    (literal-node
     (values (%advanced-consuming-result node state context :char) t))
    (char-class-node
     (values (%advanced-consuming-result node state context :class) t))
    (any-char-node
     (values (%advanced-consuming-result node state context :any) t))
    (line-break-node
     (values (%advanced-consuming-result node state context :line-break) t))
    (anchor-node (values (%advanced-anchor-result node state context) t))
    (reset-match-start-node
     (let ((result (%advanced-state-copy state)))
       (setf (advanced-state-reported-start result)
             (advanced-state-position state))
       (values (list result) t)))
    (otherwise (values nil nil))))

(defun %advanced-composite-node-result (node state context depth)
  (typecase node
    (concat-node
     (values
      (%advanced-evaluate-concat (concat-node-children node) state context depth)
      t))
    (alternation-node
     (values (%advanced-evaluate-alternation
              (alternation-node-branches node) state context depth)
             t))
    (possessive-repetition-node
     (values (%advanced-repeat-results node state context depth t) t))
    (repetition-node
     (values (%advanced-repeat-results node state context depth nil) t))
    (group-node (values (%advanced-group-result node state context depth) t))
    (assertion-node
     (values (%advanced-assertion-result node state context depth) t))
    (atomic-node (values (%advanced-atomic-result node state context depth) t))
    (otherwise (values nil nil))))

(defun %advanced-special-node-result (node state context depth)
  (typecase node
    (backreference-node
     (values (%advanced-backreference-result node state context depth) t))
    (grapheme-node (values (%advanced-grapheme-result node state context) t))
    (conditional-node
     (values (%advanced-conditional-result node state context depth) t))
    (subroutine-node
     (values (%advanced-subroutine-result node state context depth) t))
    (callout-node (values (%advanced-callout-result node state context) t))
    (control-verb-node (values (%advanced-control-result node state) t))
    (otherwise (values nil nil))))

(defun %advanced-node-evaluate (node state context depth)
  (%advanced-step context depth)
  (multiple-value-bind (result handled-p)
      (%advanced-basic-node-result node state context)
    (if handled-p
        result
        (multiple-value-bind (result handled-p)
            (%advanced-composite-node-result node state context depth)
          (if handled-p
              result
              (multiple-value-bind (result handled-p)
                  (%advanced-special-node-result node state context depth)
                (if handled-p
                    result
                    (error "Unknown advanced regex AST node: ~S" node))))))))
