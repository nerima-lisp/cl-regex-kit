;;;; t/nfa-test.lisp
(in-package #:cl-regex-kit/test)

(it "compiles every supported AST node to a terminating instruction program"
  (multiple-value-bind (program group-count)
      (cl-regex-kit::compile-to-nfa (cl-regex-kit::parse-regex "^(a|b)[0-9]{2,3}$") "^(a|b)[0-9]{2,3}$")
    (expect group-count :to-equal 1)
    (expect (cl-regex-kit::inst-op (aref program (1- (length program)))) :to-be :match)))

(it-property
  "every generated valid pattern compiles to a program ending in :match"
  ((pattern (gen-map (lambda (parts) (apply #'concatenate 'string parts))
                     (gen-list (gen-member '("a" "b" "." "[0-9]" "a*" "b+" "c?" "(ab)" "a|b"))
                               :min-length 1 :max-length 6)
                     :name :pattern)))
  (multiple-value-bind (program group-count)
      (cl-regex-kit::compile-to-nfa (cl-regex-kit::parse-regex pattern) pattern)
    (declare (ignore group-count))
    (expect (plusp (length program)) :to-be-truthy)
    (expect (cl-regex-kit::inst-op (aref program (1- (length program))))
            :to-be :match)))
