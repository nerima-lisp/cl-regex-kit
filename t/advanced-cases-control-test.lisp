(in-package #:cl-regex-kit/test)

(it "covers advanced anchors, controls, callouts, and conditions"
  (with-advanced-state-fixtures (context state group)
    (let ((ctx (context (concatenate 'string "a" (string #\Newline))))
          (start-node (make-instance 'cl-regex-kit::anchor-node
                                     :kind :match-start))
          (end-node (make-instance 'cl-regex-kit::anchor-node
                                   :kind :end-before-final-newline)))
      (expect (length (cl-regex-kit::%advanced-anchor-result
                       start-node (state 0 nil) ctx))
              :to-equal 1)
      (expect (cl-regex-kit::%advanced-anchor-result
               start-node (state 1 nil) ctx)
              :to-be nil)
      (expect (length (cl-regex-kit::%advanced-anchor-result
                       end-node (state 1 nil) ctx))
              :to-equal 1)
      (expect (cl-regex-kit::%advanced-anchor-result
               end-node (state 0 nil) ctx)
              :to-be nil)
      (expect (length (cl-regex-kit::%advanced-anchor-result
                       end-node (state 1 nil)
                       (context (concatenate 'string
                                             "a"
                                             (string #\Return)
                                             (string #\Newline)))))
              :to-equal 1)
      (expect (length (cl-regex-kit::%advanced-anchor-result
                       end-node (state 0 nil)
                       (context "")))
              :to-equal 1))
    (let* ((source (state 4 nil))
           (mark-node (make-instance 'cl-regex-kit::control-verb-node
                                     :verb :mark :argument "point"))
           (marked (first (cl-regex-kit::%advanced-control-result
                           mark-node source)))
           (named-skip
            (make-instance 'cl-regex-kit::control-verb-node
                           :verb :skip :argument "point"))
           (skip-nil
            (make-instance 'cl-regex-kit::control-verb-node
                           :verb :skip))
           (skip-integer
            (make-instance 'cl-regex-kit::control-verb-node
                           :verb :skip :argument 9)))
      (expect (cl-regex-kit::advanced-state-mark marked) :to-be-truthy)
      (expect (cl-regex-kit::advanced-state-skip-to
               (first (cl-regex-kit::%advanced-control-result
                       named-skip marked)))
              :to-equal 4)
      (expect (cl-regex-kit::advanced-state-skip-to
               (first (cl-regex-kit::%advanced-control-result
                       skip-nil source)))
              :to-equal 4)
      (expect (cl-regex-kit::advanced-state-skip-to
               (first (cl-regex-kit::%advanced-control-result
                       skip-integer source)))
              :to-equal 9)
      (dolist (verb '(:accept :commit :prune :then))
        (let ((result
                (first
                 (cl-regex-kit::%advanced-control-result
                  (make-instance 'cl-regex-kit::control-verb-node
                                 :verb verb :argument 3)
                  source))))
          (expect (cl-regex-kit::advanced-state-control result)
                  :to-be (if (eq verb :then) :then verb))))
      (expect (cl-regex-kit::advanced-state-committed-p
               (first (cl-regex-kit::%advanced-control-result
                       (make-instance 'cl-regex-kit::control-verb-node
                                      :verb :commit)
                       source)))
              :to-be-truthy)
      (expect (cl-regex-kit::%advanced-control-result
               (make-instance 'cl-regex-kit::control-verb-node :verb :fail)
               source)
              :to-be nil)
      (signals error
        (cl-regex-kit::%advanced-control-result
         (make-instance 'cl-regex-kit::control-verb-node :verb :unknown)
         source)))
    (let ((node (make-instance 'cl-regex-kit::callout-node
                               :number 7 :tag "check"))
          (seen nil)
          (ctx nil))
      (setf ctx (context "abc"
                         :callout
                         (lambda (number tag position text)
                           (setf seen (list number tag position text))
                           :continue)))
      (let ((state (state 1 nil)))
        (expect (length (cl-regex-kit::%advanced-callout-result
                         node state ctx))
                :to-equal 1)
        (expect seen :to-equal '(7 "check" 1 "abc")))
      (setf (cl-regex-kit::advanced-context-callout ctx)
            (lambda (number tag position text)
              (declare (ignore number tag position text))
              :fail))
      (expect (cl-regex-kit::%advanced-callout-result node (state 0 nil) ctx)
              :to-be nil)
      (setf (cl-regex-kit::advanced-context-callout ctx)
            (lambda (number tag position text)
              (declare (ignore number tag position text))
              :invalid))
      (signals error
        (cl-regex-kit::%advanced-callout-result node (state 0 nil) ctx))
      (setf (cl-regex-kit::advanced-context-callout ctx) nil)
      (expect (length (cl-regex-kit::%advanced-callout-result
                       node (state 0 nil) ctx))
              :to-equal 1))
    (let* ((group (make-instance 'cl-regex-kit::group-node
                                 :name "word"
                                 :capture-index 2
                                 :child (make-instance 'cl-regex-kit::literal-node
                                                       :char #\a)))
           (root (make-instance 'cl-regex-kit::concat-node
                                :children (list group)))
           (ctx (context "a" :root root))
           (slots (make-array 8
                              :initial-contents '(nil nil nil nil 0 1 nil nil)))
           (normal (state 0 slots))
           (recursive (state 0 slots
                             :recursion-depth 1
                             :recursion-target group)))
      (expect (cl-regex-kit::%advanced-condition-true-p
               :define normal ctx 0)
              :to-be nil)
      (expect (cl-regex-kit::%advanced-condition-true-p
               :recursion normal ctx 0)
              :to-be nil)
      (expect (cl-regex-kit::%advanced-condition-true-p
               :recursion recursive ctx 0)
              :to-be-truthy)
      (expect (cl-regex-kit::%advanced-condition-true-p
               '(:recursion-index 2) recursive ctx 0)
              :to-be-truthy)
      (expect (cl-regex-kit::%advanced-condition-true-p
               '(:recursion-name "word") recursive ctx 0)
              :to-be-truthy)
      (expect (cl-regex-kit::%advanced-condition-true-p
               '(:capture-index 2) normal ctx 0)
              :to-be-truthy)
      (expect (cl-regex-kit::%advanced-condition-true-p
               '(:name "word") normal ctx 0)
              :to-be-truthy)
      (expect (cl-regex-kit::%advanced-condition-true-p
               (make-instance 'cl-regex-kit::literal-node :char #\a)
               normal ctx 0)
              :to-be-truthy)
      (expect (cl-regex-kit::%advanced-condition-true-p
               :unknown normal ctx 0)
              :to-be nil)
      (expect (first (cl-regex-kit::%advanced-optional-node-evaluate
                      nil normal ctx 0))
              :to-be normal)
      (expect (first (cl-regex-kit::%advanced-optional-node-evaluate
                      (make-instance 'cl-regex-kit::literal-node :char #\a)
                      normal ctx 0))
              :to-be-truthy)
      (dolist (case
               (list
                (cons (make-instance 'cl-regex-kit::subroutine-node
                                     :target group)
                      group)
                (cons (make-instance 'cl-regex-kit::subroutine-node
                                     :target 0)
                      root)
                (cons (make-instance 'cl-regex-kit::subroutine-node
                                     :target 2)
                      group)
                (cons (make-instance 'cl-regex-kit::subroutine-node
                                     :target "word")
                      group)
                (cons (make-instance 'cl-regex-kit::subroutine-node
                                     :target 'word)
                      group)
                (cons (make-instance 'cl-regex-kit::recursion-node)
                      root)))
        (expect (cl-regex-kit::%advanced-subroutine-target (car case) ctx)
                :to-be (cdr case)))
      (expect (cl-regex-kit::%advanced-subroutine-target
               (make-instance 'cl-regex-kit::subroutine-node :target nil)
               ctx)
              :to-be nil)
      (signals error
        (cl-regex-kit::%advanced-subroutine-result
         (make-instance 'cl-regex-kit::subroutine-node :target "missing")
         normal ctx 0)))))
