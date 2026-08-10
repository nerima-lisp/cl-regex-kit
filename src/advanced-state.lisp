(in-package #:cl-regex-kit)

(defstruct (advanced-state
            (:constructor
             %make-advanced-state
             (position slots &optional control skip-to mark reported-start
                       recursion-depth recursion-target committed-p))) position
  slots
  control
  skip-to
  mark
  reported-start
  recursion-depth
  recursion-target
  committed-p)

(defstruct advanced-context text
  search-start
  limit
  text-length
  byte-mode-p
  never-newline-p
  root
  group-count
  group-names
  step-limit
  steps
  nest-limit
  callout
  (recursion-stack nil)
  (boundary-cache nil))

(defstruct advanced-grapheme-unit character
  start
  end
  class
  indic-conjunct-break
  extended-pictographic-p)

(define-condition advanced-regex-limit-error (cl-regex-kit-error)
  ((kind :initarg :kind :reader advanced-regex-limit-kind)
   (limit :initarg :limit :reader advanced-regex-limit)
   (used :initarg :used :reader advanced-regex-limit-used))
  (:report
   (lambda (condition stream)
     (format
      stream
      "Advanced regex ~A limit ~D exceeded after ~D units."
      (advanced-regex-limit-kind condition)
      (advanced-regex-limit condition)
      (advanced-regex-limit-used condition)))))

(progn
  (defun %advanced-copy-capture-stack (stack)
    (mapcar
     (lambda (entry)
       (cons (car entry) (cdr entry)))
     stack))
  (defun %advanced-copy-capture-stacks (stacks)
    (when stacks
      (map (quote vector)
           (function %advanced-copy-capture-stack)
           stacks)))
  (defun %advanced-copy-slots (slots)
    (let ((copy (copy-seq slots)))
      (when (and (plusp (length copy))
                 (vectorp (aref copy (1- (length copy)))))
        (setf (aref copy (1- (length copy)))
              (%advanced-copy-capture-stacks
               (aref copy (1- (length copy))))))
      copy))
  (defun %advanced-initialize-capture-stacks (slots highest-capture)
  (setf (aref slots (* 2 (1+ highest-capture)))
        (make-array (1+ highest-capture) :initial-element nil))
  slots)
  (defun %advanced-capture-stacks (state)
    (let ((slots (advanced-state-slots state)))
      (and (plusp (length slots))
           (let ((stacks (aref slots (1- (length slots)))))
             (and (vectorp stacks) stacks)))))
  (defun %advanced-sync-capture-slot (state index)
    (let* ((stacks (%advanced-capture-stacks state))
           (stack (and stacks (aref stacks index)))
           (top (first stack))
           (slots (advanced-state-slots state)))
      (setf (aref slots (* 2 index)) (and top (car top))
            (aref slots (1+ (* 2 index))) (and top (cdr top)))
      state)))

(defun %advanced-state-copy (state)
  (%make-advanced-state
   (advanced-state-position state)
   (%advanced-copy-slots (advanced-state-slots state))
   (advanced-state-control state)
   (advanced-state-skip-to state)
   (advanced-state-mark state)
   (advanced-state-reported-start state)
   (advanced-state-recursion-depth state)
   (advanced-state-recursion-target state)
   (advanced-state-committed-p state)))

(defun %advanced-recursion-active-p (target position context)
  (some
   (lambda (entry)
     (and (eq (car entry) target)
          (= (cdr entry) position)))
   (advanced-context-recursion-stack context)))

(defun %advanced-state-terminal-p (state)
  (member
   (advanced-state-control state)
   (list :accept :commit :commit-failure :prune :skip :then)
   :test
   (function eq)))

(defun %advanced-state-normal-p (state)
  (null (advanced-state-control state)))

(defun %advanced-step (context depth)
  (incf (advanced-context-steps context))
  (when (> depth (advanced-context-nest-limit context))
    (error
     'advanced-regex-limit-error
     :kind
     :nest-depth
     :limit
     (advanced-context-nest-limit context)
     :used
     depth))
  (when (>
         (advanced-context-steps context)
         (advanced-context-step-limit context))
    (error
     'advanced-regex-limit-error
     :kind
     :steps
     :limit
     (advanced-context-step-limit context)
     :used
     (advanced-context-steps context))))
