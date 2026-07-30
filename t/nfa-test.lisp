;;;; t/nfa-test.lisp
(in-package #:cl-regex-kit/test)

(it "compiles every supported AST node to a terminating instruction program"
  (multiple-value-bind (program group-count)
      (cl-regex-kit::compile-to-nfa (cl-regex-kit::parse-regex "^(a|b)[0-9]{2,3}$") "^(a|b)[0-9]{2,3}$")
    (expect group-count :to-equal 1)
    (expect (cl-regex-kit::inst-op (aref program (1- (length program)))) :to-be :match)))
