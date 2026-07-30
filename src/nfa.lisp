;;;; src/nfa.lisp
;;;;
;;;; A REGEX-NODE tree -> a compiled program: a vector of INST, following
;;;; Thompson's construction (Russell Cox, "Regular Expression Matching Can Be
;;;; Simple and Fast"). PIKE-VM simulates this program without ever
;;;; backtracking, which is what keeps matching linear in the input length.
;;;;
;;;; NOT YET IMPLEMENTED. Left as a stub so the system loads and the test
;;;; harness runs; replace the body below with a real compiler.
(in-package #:cl-regex-kit)

(defstruct inst
  "One instruction in a compiled Thompson-NFA program.
OP is one of :CHAR, :CLASS, :ANY, :SPLIT, :JMP, :SAVE, :MATCH.
A and B are op-specific: a character or CHAR-CLASS-NODE, a jump target index,
or a capture-slot index. :SPLIT branches to both A and B, trying A first so
priority order (greedy vs. lazy, leftmost-first alternation) is preserved."
  op a b)

(defun compile-to-nfa (ast)
  "Compile AST (a REGEX-NODE tree) into a program: a vector of INST.
Returns (VALUES PROGRAM GROUP-COUNT)."
  (declare (ignore ast))
  (error "COMPILE-TO-NFA is not yet implemented."))
