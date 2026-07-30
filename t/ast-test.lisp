;;;; t/ast-test.lisp
(in-package #:cl-regex-kit/test)

(it "constructs a literal node holding its character"
  (let ((node (make-instance 'cl-regex-kit::literal-node :char #\a)))
    (expect (cl-regex-kit::literal-node-char node) :to-be #\a)))

(it "defaults a repetition node to greedy with an unbounded max"
  (let ((node (make-instance 'cl-regex-kit::repetition-node
                              :child (make-instance 'cl-regex-kit::literal-node :char #\a)
                              :min 0)))
    (expect (cl-regex-kit::repetition-node-max node) :to-be nil)
    (expect (cl-regex-kit::repetition-node-greedy-p node) :to-be-truthy)))

(it "defaults a group node to non-capturing"
  (let ((node (make-instance 'cl-regex-kit::group-node
                              :child (make-instance 'cl-regex-kit::any-char-node))))
    (expect (cl-regex-kit::group-node-capture-index node) :to-be nil)))

(it "defaults a character class node to not negated"
  (let ((node (make-instance 'cl-regex-kit::char-class-node
                              :ranges (list (cons #\a #\z)))))
    (expect (cl-regex-kit::char-class-node-negated-p node) :to-be nil)))
