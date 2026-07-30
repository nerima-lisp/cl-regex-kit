;;;; t/nfa-test.lisp
(in-package #:cl-regex-kit/test)

;; COMPILE-TO-NFA (src/nfa.lisp) has no implementation yet. Replace this
;; placeholder with real specs once AST nodes compile to an INST program --
;; one spec per node type is a reasonable starting split.
(it "signals that it is not yet implemented"
  (signals error (cl-regex-kit::compile-to-nfa (make-instance 'cl-regex-kit::any-char-node))))
