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

;;; COMPILE-TO-NFA's Thompson-construction state -- the growing instruction
;;; vector, and the pattern/limit an overflow needs to report -- lives in
;;; these dynamically-bound special variables rather than a closure over a
;;; ~13-function LABELS block, the same technique the parser
;;; (parser-syntax.lisp/regex-grammar.lisp) uses for its own shared state:
;;; it is what lets EMIT/PATCH/JOIN/ALTERNATE/... be ordinary top-level
;;; DEFUNs instead of one large nested form. Each COMPILE-TO-NFA call binds
;;; a fresh set via LET*, so concurrent calls never share a binding.
(defvar *nfa-instructions*)
(defvar *nfa-pattern*)
(defvar *nfa-instruction-limit*)

(defun emit (op &optional a b c)
  (when (>= (length *nfa-instructions*) *nfa-instruction-limit*)
    (error 'regex-syntax-error
           :pattern *nfa-pattern*
           :reason "Regular expression exceeds the configured NFA instruction limit"))
  (vector-push-extend (make-inst :op op :a a :b b :c c) *nfa-instructions*)
  (1- (length *nfa-instructions*)))

(defun out (index slot) (list (cons index slot)))

(defun patch (outs target)
  (dolist (entry outs)
    (ecase (cdr entry)
      (:a (setf (inst-a (aref *nfa-instructions* (car entry))) target))
      (:b (setf (inst-b (aref *nfa-instructions* (car entry))) target)))))

(defun empty-fragment ()
  (let ((index (emit :jmp)))
    (make-fragment :start index :outs (out index :a))))

(defun join (left right)
  (patch (fragment-outs left) (fragment-start right))
  (make-fragment :start (fragment-start left) :outs (fragment-outs right)))

(defun alternate (left right)
  (let ((index (emit :split (fragment-start left) (fragment-start right))))
    (make-fragment :start index
                   :outs (nconc (fragment-outs right) (fragment-outs left)))))

(defun optional-fragment (fragment greedy-p)
  (let ((index (if greedy-p
                   (emit :split (fragment-start fragment))
                   (emit :split nil (fragment-start fragment)))))
    (make-fragment :start index
                   :outs (nconc (fragment-outs fragment)
                                (out index (if greedy-p :b :a))))))

(defun star (fragment greedy-p)
  (let ((index (if greedy-p
                   (emit :split (fragment-start fragment))
                   (emit :split nil (fragment-start fragment)))))
    (patch (fragment-outs fragment) index)
    (make-fragment :start index :outs (out index (if greedy-p :b :a)))))

(defun compile-repetition (node)
  (let ((result (empty-fragment))
        (child (repetition-node-child node))
        (minimum (repetition-node-min node))
        (maximum (repetition-node-max node))
        (greedy-p (repetition-node-greedy-p node)))
    (dotimes (ignored minimum)
      (setf result (join result (compile-node child))))
    (if maximum
        (dotimes (ignored (- maximum minimum))
          (setf result (join result (optional-fragment (compile-node child) greedy-p))))
        (setf result (join result (star (compile-node child) greedy-p))))
    result))

(defun anchor-kind-op (kind)
  (ecase kind
    (:start :bol)
    (:end :eol)
    (:absolute-start :bos)
    (:absolute-end :eos)
    (:word-boundary :boundary)
    (:not-word-boundary :non-boundary)
    (:word-start :word-start)
    (:word-end :word-end)
    (:word-start-half :word-start-half)
    (:word-end-half :word-end-half)))

(defun compile-anchor (node)
  (let ((index (emit (anchor-kind-op (anchor-node-kind node))
                      nil
                      (logior (if (anchor-node-multiline-p node) 1 0)
                              (if (anchor-node-unicode-p node) 2 0)
                              (if (anchor-node-crlf-p node) 4 0))
                      (anchor-node-line-terminator node))))
    (make-fragment :start index :outs (out index :a))))

(defun compile-group (node)
  (let* ((capture-index (group-node-capture-index node))
         (child (compile-node (group-node-child node))))
    (if capture-index
        (let ((begin (emit :save (* 2 capture-index)))
              (end (emit :save (1+ (* 2 capture-index)))))
          (patch (out begin :b) (fragment-start child))
          (patch (fragment-outs child) end)
          (make-fragment :start begin :outs (out end :b)))
        child)))

(defun compile-node (node)
  (typecase node
    (literal-node (let ((index (emit :char node))) (make-fragment :start index :outs (out index :b))))
    (char-class-node (let ((index (emit :class node))) (make-fragment :start index :outs (out index :b))))
    (any-char-node (let ((index (emit :any node))) (make-fragment :start index :outs (out index :b))))
    (anchor-node (compile-anchor node))
    (concat-node
     (reduce #'join (mapcar #'compile-node (concat-node-children node)) :initial-value (empty-fragment)))
    (alternation-node (reduce #'alternate (mapcar #'compile-node (alternation-node-branches node))))
    (repetition-node (compile-repetition node))
    (group-node (compile-group node))
    (otherwise (error "Unknown regex AST node: ~S" node))))

(defun max-capture-index (node)
  (typecase node
    (group-node
     (max (or (group-node-capture-index node) 0) (max-capture-index (group-node-child node))))
    (concat-node
     (reduce #'max (mapcar #'max-capture-index (concat-node-children node)) :initial-value 0))
    (alternation-node
     (reduce #'max (mapcar #'max-capture-index (alternation-node-branches node)) :initial-value 0))
    (repetition-node (max-capture-index (repetition-node-child node)))
    (otherwise 0)))

(defun compile-to-nfa (ast pattern &key (instruction-limit +maximum-instruction-count+))
  "Compile AST into a vector of INST and return it with its capture count."
  (check-type instruction-limit (integer 1 *))
  (let* ((*nfa-instructions* (make-array 32 :adjustable t :fill-pointer 0))
         (*nfa-pattern* pattern)
         (*nfa-instruction-limit* instruction-limit)
         (group-count (max-capture-index ast))
         (begin (emit :save 0))
         (body (compile-node ast))
         (end (emit :save 1))
         (match (emit :match)))
    (patch (out begin :b) (fragment-start body))
    (patch (fragment-outs body) end)
    (patch (out end :b) match)
    ;; *NFA-INSTRUCTIONS* is adjustable/fill-pointered to support
    ;; VECTOR-PUSH-EXTEND during construction, but every reader of a REGEX's
    ;; PROGRAM (the Pike VM's per-instruction, per-character AREF in
    ;; pike-vm-closure.lisp/pike-vm-capture.lisp/pike-vm-set.lisp) is
    ;; read-only from here on -- coercing once to a SIMPLE-VECTOR removes
    ;; that indirection from the hottest AREF in the engine.
    (values (coerce *nfa-instructions* 'simple-vector) group-count)))
