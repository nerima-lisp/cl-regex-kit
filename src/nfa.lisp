;;;; src/nfa.lisp
(in-package #:cl-regex-kit)

(defstruct inst op a b c)

(defstruct fragment start outs)

(defconstant +maximum-instruction-count+ 100000 "Largest NFA program accepted during compilation.")

(defun merge-nfa-programs (programs)
  "Merge PROGRAMS under one entry point for capture-free set matching.

Each source program's :MATCH instruction becomes :SET-MATCH whose A operand is
the source program's zero-based index.  All control-flow targets are relocated
without changing the immutable AST nodes referenced by consuming instructions."
  (let* ((program-vector (coerce programs 'vector))
         (count (length program-vector))
         (root-size (if (plusp count) count 0))
         (offsets (make-array count))
         (next-offset root-size))
    (dotimes (index count)
      (setf (aref offsets index) next-offset)
      (incf next-offset (length (aref program-vector index))))
    (let ((merged (make-array next-offset)))
      (labels ((target (offset value)
                 (and value (+ offset value)))
               (relocate (instruction offset pattern-index)
                 (case (inst-op instruction)
                   (:split
                    (make-inst :op :split
                               :a (target offset (inst-a instruction))
                               :b (target offset (inst-b instruction))))
                   (:match (make-inst :op :set-match :a pattern-index))
                   (:jmp
                    (make-inst :op :jmp
                               :a (target offset (inst-a instruction))))
                   (:save
                    (make-inst :op :save
                               :a (inst-a instruction)
                               :b (target offset (inst-b instruction))))
                   ((:char :class :any)
                   (make-inst :op (inst-op instruction)
                               :a (inst-a instruction)
                               :b (target offset (inst-b instruction))
                               :c (inst-c instruction)))
                   ((:bol :eol :bos :eos :boundary :non-boundary
                     :word-start :word-end :word-start-half :word-end-half)
                   (make-inst :op (inst-op instruction)
                               :a (target offset (inst-a instruction))
                               :b (inst-b instruction)
                               :c (inst-c instruction)))
                   (otherwise
                    (error "Cannot merge NFA instruction: ~S" instruction)))))
        (when (plusp count)
          (dotimes (index (1- count))
            (setf (aref merged index)
                  (make-inst :op :split
                             :a (aref offsets index)
                             :b (1+ index))))
          (setf (aref merged (1- count))
                (make-inst :op :jmp :a (aref offsets (1- count)))))
        (dotimes (pattern-index count)
          (let ((program (aref program-vector pattern-index))
                (offset (aref offsets pattern-index)))
            (dotimes (instruction-index (length program))
              (setf (aref merged (+ offset instruction-index))
                    (relocate (aref program instruction-index)
                              offset
                              pattern-index)))))
        merged))))

(defun compile-to-nfa (ast pattern
                       &key (instruction-limit +maximum-instruction-count+))
  "Compile AST into a vector of INST and return it with its capture count."
  (check-type instruction-limit (integer 1 *))
  (let ((instructions (make-array 32 :adjustable t :fill-pointer 0)))
    (labels ((emit (op &optional a b c)
             (when (>= (length instructions) instruction-limit)
               (error (quote regex-syntax-error)
                      :pattern pattern
                      :reason "Regular expression exceeds the configured NFA instruction limit"))
             (vector-push-extend (make-inst :op op :a a :b b :c c) instructions)
             (1- (length instructions)))
             (out (index slot) (list (cons index slot)))
             (patch (outs target)
               (dolist (entry outs)
                 (ecase (cdr entry)
                   (:a (setf (inst-a (aref instructions (car entry))) target))
                   (:b (setf (inst-b (aref instructions (car entry))) target)))))
             (empty ()
               (let ((index (emit :jmp)))
                 (make-fragment :start index :outs (out index :a))))
             (join (left right)
               (patch (fragment-outs left) (fragment-start right))
               (make-fragment :start (fragment-start left) :outs (fragment-outs right)))
             (alternate (left right)
               (let ((index (emit :split (fragment-start left) (fragment-start right))))
                 (make-fragment :start index
                                :outs (nconc (fragment-outs left) (fragment-outs right)))))
             (optional (fragment greedy-p)
               (let ((index (if greedy-p
                                (emit :split (fragment-start fragment))
                                (emit :split nil (fragment-start fragment)))))
                 (make-fragment :start index
                                :outs (nconc (fragment-outs fragment)
                                             (out index (if greedy-p :b :a))))))
             (star (fragment greedy-p)
               (let ((index (if greedy-p
                                (emit :split (fragment-start fragment))
                                (emit :split nil (fragment-start fragment)))))
                 (patch (fragment-outs fragment) index)
                 (make-fragment :start index :outs (out index (if greedy-p :b :a)))))
             (compile-repetition (node)
               (let ((result (empty))
                     (child (repetition-node-child node))
                     (minimum (repetition-node-min node))
                     (maximum (repetition-node-max node))
                     (greedy-p (repetition-node-greedy-p node)))
                 (dotimes (ignored minimum)
                                      (setf result (join result (compile-node child))))
                 (if maximum
                     (dotimes (ignored (- maximum minimum))
                                              (setf result (join result (optional (compile-node child) greedy-p))))
                     (setf result (join result (star (compile-node child) greedy-p))))
                 result))
             (compile-node (node)
               (typecase node
                 (literal-node
                  (let ((index (emit :char node)))
                    (make-fragment :start index :outs (out index :b))))
                 (char-class-node
                  (let ((index (emit :class node)))
                    (make-fragment :start index :outs (out index :b))))
                 (any-char-node
                  (let ((index (emit :any node)))
                    (make-fragment :start index :outs (out index :b))))
                 (anchor-node
                  (let ((index (emit (ecase (anchor-node-kind node)
                                       (:start :bol)
                                       (:end :eol)
                                       (:absolute-start :bos)
                                       (:absolute-end :eos)
                                       (:word-boundary :boundary)
                                       (:not-word-boundary :non-boundary)
                                       (:word-start :word-start)
                                       (:word-end :word-end)
                                       (:word-start-half :word-start-half)
                                       (:word-end-half :word-end-half))
                                     nil
                                     (logior (if (anchor-node-multiline-p node) 1 0)
                                             (if (anchor-node-unicode-p node) 2 0)
                                             (if (anchor-node-crlf-p node) 4 0))
                                     (anchor-node-line-terminator node))))
                    (make-fragment :start index :outs (out index :a))))
                 (concat-node
                   (reduce #'join (mapcar #'compile-node (concat-node-children node))
                           :initial-value (empty)))
                 (alternation-node
                  (reduce #'alternate
                          (mapcar #'compile-node (alternation-node-branches node))))
                 (repetition-node (compile-repetition node))
                 (group-node
                  (let* ((capture-index (group-node-capture-index node))
                         (child (compile-node (group-node-child node))))
                    (if capture-index
                        (let ((begin (emit :save (* 2 capture-index)))
                              (end (emit :save (1+ (* 2 capture-index)))))
                          (patch (out begin :b) (fragment-start child))
                          (patch (fragment-outs child) end)
                          (make-fragment :start begin :outs (out end :b)))
                        child)))
                 (otherwise (error "Unknown regex AST node: ~S" node))))
             (max-capture-index (node)
               (typecase node
                 (group-node (max (or (group-node-capture-index node) 0)
                                  (max-capture-index (group-node-child node))))
                 (concat-node (reduce #'max (mapcar #'max-capture-index (concat-node-children node)) :initial-value 0))
                 (alternation-node (reduce #'max (mapcar #'max-capture-index (alternation-node-branches node)) :initial-value 0))
                 (repetition-node (max-capture-index (repetition-node-child node)))
                 (otherwise 0))))
      (let* ((group-count (max-capture-index ast))
             (begin (emit :save 0))
             (body (compile-node ast))
             (end (emit :save 1))
             (match (emit :match)))
        (patch (out begin :b) (fragment-start body))
        (patch (fragment-outs body) end)
        (patch (out end :b) match)
        (values instructions group-count)))))
