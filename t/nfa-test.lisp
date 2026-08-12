;;;; t/nfa-test.lisp
(in-package #:cl-regex-kit/test)

(it
  "compiles every supported AST node to a terminating instruction program"
  (multiple-value-bind (program group-count) (cl-regex-kit::compile-to-nfa
      (cl-regex-kit::parse-regex "^(a|b)[0-9]{2,3}$")
      "^(a|b)[0-9]{2,3}$")
    (expect group-count :to-equal 1)
    (expect
      (cl-regex-kit::inst-op (aref program (1- (length program))))
      :to-be
      :match)))

(it-property
  "every generated valid pattern compiles to a program ending in :match"
  ((pattern
      (gen-map
        (lambda (parts)
          (apply #'concatenate 'string parts))
        (gen-list
          (gen-member '("a" "b" "." "[0-9]" "a*" "b+" "c?" "(ab)" "a|b"))
          :min-length
          1
          :max-length
          6)
        :name
        :pattern)))
  (multiple-value-bind (program group-count) (cl-regex-kit::compile-to-nfa (cl-regex-kit::parse-regex pattern) pattern)
    (declare (ignore group-count))
    (expect (plusp (length program)) :to-be-truthy)
    (expect
      (cl-regex-kit::inst-op (aref program (1- (length program))))
      :to-be
      :match)))

(it
  "preserves alternation priority while compiling many branches linearly"
  (let* ((program (cl-regex-kit::regex-program (compile-regex "a|b|c")))
         (entry (cl-regex-kit::inst-b (aref program 0)))
         (root (aref program entry)))
    (expect (cl-regex-kit::inst-op root) :to-be :split)
    (expect
      (cl-regex-kit::inst-op (aref program (cl-regex-kit::inst-a root)))
      :to-be
      :split)
    (expect
      (cl-regex-kit::literal-node-char
        (cl-regex-kit::inst-a (aref program (cl-regex-kit::inst-b root))))
      :to-be
      #\c))
  (let* ((branch-count 2048)
         (branches
        (loop repeat branch-count
              collect (make-instance (quote cl-regex-kit::literal-node) :char #\a)))
         (ast (make-instance (quote cl-regex-kit::alternation-node) :branches branches)))
    (multiple-value-bind (program group-count) (cl-regex-kit::compile-to-nfa ast "large alternation")
      (expect group-count :to-equal 0)
      (expect (length program) :to-equal (+ (* 2 branch-count) 2))
      (expect
        (count :split program :key (function cl-regex-kit::inst-op))
        :to-equal
        (1- branch-count))
      (expect
        (every
          (lambda (instruction)
            (or
              (not (eq (cl-regex-kit::inst-op instruction) :char))
              (integerp (cl-regex-kit::inst-b instruction))))
          program)
        :to-be-truthy))))

(it
  "merges NFA programs through the owned executor path and rejects unknown nodes"
  (let* ((cl-regex-kit::*nfa-merge-parallelism-override* 2)
         (programs
           (vector
             (vector
               (cl-regex-kit::make-inst
                 :op :char
                 :a (make-instance 'cl-regex-kit::literal-node :char #\a)
                 :b 1)
               (cl-regex-kit::make-inst :op :match))
             (vector
               (cl-regex-kit::make-inst
                 :op :char
                 :a (make-instance 'cl-regex-kit::literal-node :char #\b)
                 :b 1)
               (cl-regex-kit::make-inst :op :match))))
         (offsets #(2 4))
         (merged (make-array 6 :initial-element nil)))
    (cl-regex-kit::merge-programs-into programs offsets merged)
    (expect (cl-regex-kit::inst-op (aref merged 2)) :to-be :char)
    (expect (cl-regex-kit::inst-b (aref merged 2)) :to-be 3)
    (expect (cl-regex-kit::inst-op (aref merged 3)) :to-be :set-match)
    (expect (cl-regex-kit::inst-a (aref merged 3)) :to-be 0)
    (expect (cl-regex-kit::inst-op (aref merged 4)) :to-be :char)
    (expect (cl-regex-kit::inst-b (aref merged 4)) :to-be 5)
    (expect (cl-regex-kit::inst-op (aref merged 5)) :to-be :set-match)
    (expect (cl-regex-kit::inst-a (aref merged 5)) :to-be 1))
  (signals
    error
    (cl-regex-kit::relocate-nfa-instruction
      (cl-regex-kit::make-inst :op :bogus)
      0
      0))
  (signals
    error
    (cl-regex-kit::compile-node
      (make-instance 'cl-regex-kit::regex-node))))
