(in-package #:cl-regex-kit/test)

(it "covers advanced evaluator control and boundary branches"
  (with-advanced-evaluator-fixtures (context state sentence-units)
    (let* ((leaf (make-instance 'cl-regex-kit::group-node
                                :name "target"
                                :capture-index 7
                                :child (make-instance
                                        'cl-regex-kit::literal-node
                                        :char #\a)))
           (miss (make-instance 'cl-regex-kit::literal-node :char #\b)))
      (dolist (node
               (list
                (make-instance 'cl-regex-kit::concat-node
                               :children (list miss leaf))
                (make-instance 'cl-regex-kit::alternation-node
                               :branches (list miss leaf))
                (make-instance 'cl-regex-kit::repetition-node
                               :child leaf :min 0 :max 1)
                (make-instance 'cl-regex-kit::possessive-repetition-node
                               :child leaf :min 0 :max 1)
                (make-instance 'cl-regex-kit::assertion-node
                               :child leaf)
                (make-instance 'cl-regex-kit::atomic-node :child leaf)
                (make-instance 'cl-regex-kit::conditional-node
                               :condition nil
                               :yes-branch miss
                               :no-branch leaf)))
        (expect (cl-regex-kit::%advanced-find-group
                 node :name "target")
                :to-be leaf)))

    (expect (cl-regex-kit::%advanced-sentence-significant-before
             (sentence-units :extend :aterm) 0)
            :to-be nil)
    (expect (cl-regex-kit::%advanced-sentence-terminal-prefix-p
             (sentence-units :sterm :close :sp :upper) 2 t)
            :to-be-truthy)
    (expect (cl-regex-kit::%advanced-sentence-terminal-prefix-p
             (sentence-units :upper) 0 nil)
            :to-be nil)
    (expect (cl-regex-kit::%advanced-sentence-aterm-before-lower-p
             (sentence-units :aterm :extend :lower) 1)
            :to-be-truthy)

    (let ((ctx (context "ab")))
      (expect (length
               (cl-regex-kit::%advanced-anchor-result
                (make-instance 'cl-regex-kit::anchor-node
                               :kind :end-before-final-newline)
                (state 2 nil)
                ctx))
              :to-equal 1))
    (let* ((node (make-instance 'cl-regex-kit::control-verb-node
                                :verb :skip
                                :argument "missing"))
           (result (first (cl-regex-kit::%advanced-control-result
                           node
                           (state 0 nil)))))
      (expect (cl-regex-kit::advanced-state-control result)
              :to-be :skip)
      (expect (cl-regex-kit::advanced-state-skip-to result)
              :to-be nil))
    (let ((result
            (first
             (cl-regex-kit::%advanced-control-result
              (make-instance 'cl-regex-kit::control-verb-node
                             :verb :skip
                             :argument #\x)
              (state 0 nil)))))
      (expect (cl-regex-kit::advanced-state-skip-to result)
              :to-be nil))

    (let ((ctx (context "a")))
      (let ((result
              (first
               (cl-regex-kit::%advanced-evaluate-concat
                nil
                (state 0 nil :control :commit)
                ctx
                0))))
        (expect (cl-regex-kit::advanced-state-control result)
                :to-be nil)
        (expect (cl-regex-kit::advanced-state-committed-p result)
                :to-be-truthy))
      (let ((result
              (first
               (cl-regex-kit::%advanced-evaluate-concat
                (list
                 (make-instance 'cl-regex-kit::control-verb-node
                                :verb :commit)
                 (make-instance 'cl-regex-kit::literal-node :char #\b))
                (state 0 nil)
                ctx
                0))))
        (expect (cl-regex-kit::advanced-state-control result)
                :to-be :commit-failure)))

    (let ((ctx (context "a")))
      (let ((result
              (first
               (cl-regex-kit::%advanced-repeat-results
                (make-instance 'cl-regex-kit::possessive-repetition-node
                               :child (make-instance
                                       'cl-regex-kit::literal-node
                                       :char #\a)
                               :min 0
                               :max 0)
                (state 0 nil)
                ctx
                0
                t))))
        (expect (cl-regex-kit::advanced-state-position result)
                :to-equal 0))
      (let ((result
              (first
               (cl-regex-kit::%advanced-repeat-results
                (make-instance 'cl-regex-kit::possessive-repetition-node
                               :child (make-instance
                                       'cl-regex-kit::control-verb-node
                                       :verb :accept)
                               :min 0)
                (state 0 nil)
                ctx
                0
                t))))
        (expect (cl-regex-kit::advanced-state-control result)
                :to-be :accept))
      (let ((result
              (first
               (cl-regex-kit::%advanced-repeat-results
                (make-instance 'cl-regex-kit::repetition-node
                               :child (make-instance
                                       'cl-regex-kit::literal-node
                                       :char #\a)
                               :min 0
                               :max 1
                               :greedy-p nil)
                (state 0 nil)
                ctx
                0
                nil))))
        (expect (cl-regex-kit::advanced-state-position result)
                :to-equal 0)))

    (let ((group (make-instance 'cl-regex-kit::group-node
                                :capture-index 1
                                :child (make-instance
                                        'cl-regex-kit::literal-node
                                        :char #\a)))
          (ctx (context "a")))
      (expect (cl-regex-kit::%advanced-group-result
               group
               (state 0 (make-array 4 :initial-element nil))
               ctx
               0)
              :to-be nil))

    (let ((ctx (context "a")))
      (let ((forward (make-instance 'cl-regex-kit::assertion-node
                                    :child (make-instance
                                            'cl-regex-kit::literal-node
                                            :char #\a)
                                    :direction :forward))
            (negative (make-instance 'cl-regex-kit::assertion-node
                                     :child (make-instance
                                             'cl-regex-kit::literal-node
                                             :char #\b)
                                     :negative-p t
                                     :direction :forward))
            (non-atomic (make-instance 'cl-regex-kit::assertion-node
                                       :child (make-instance
                                               'cl-regex-kit::alternation-node
                                               :branches
                                               (list
                                                (make-instance
                                                 'cl-regex-kit::literal-node
                                                 :char #\a)
                                                (make-instance
                                                 'cl-regex-kit::literal-node
                                                 :char #\a)))
                                       :non-atomic-p t
                                       :direction :forward))
            (backward (make-instance 'cl-regex-kit::assertion-node
                                     :child (make-instance
                                             'cl-regex-kit::literal-node
                                             :char #\a)
                                     :direction :backward
                                     :fixed-length 1))
            (backward-negative (make-instance
                                'cl-regex-kit::assertion-node
                                :child (make-instance
                                        'cl-regex-kit::literal-node
                                        :char #\b)
                                :direction :backward
                                :fixed-length 1
                                :negative-p t)))
        (expect (length (cl-regex-kit::%advanced-assertion-result
                         forward (state 0 nil) ctx 0))
                :to-equal 1)
        (expect (length (cl-regex-kit::%advanced-assertion-result
                         negative (state 0 nil) ctx 0))
                :to-equal 1)
        (expect (length (cl-regex-kit::%advanced-assertion-result
                         non-atomic (state 0 nil) ctx 0))
                :to-equal 2)
        (expect (length (cl-regex-kit::%advanced-assertion-result
                         backward (state 1 nil) ctx 0))
                :to-equal 1)
        (expect (length (cl-regex-kit::%advanced-assertion-result
                         backward-negative (state 1 nil) ctx 0))
                :to-equal 1))

    (let ((ctx (context "a")))
      (expect (length
               (cl-regex-kit::%advanced-assertion-result
                (make-instance 'cl-regex-kit::assertion-node
                               :child (make-instance
                                       'cl-regex-kit::control-verb-node
                                       :verb :accept)
                               :direction :forward)
                (state 0 nil)
                ctx
                0))
              :to-equal 1)
      (expect (length
               (cl-regex-kit::%advanced-assertion-result
                (make-instance 'cl-regex-kit::assertion-node
                               :child (make-instance
                                       'cl-regex-kit::literal-node
                                       :char #\a)
                               :direction :backward)
                (state 1 nil)
                ctx
                0))
              :to-equal 1))

    (expect (cl-regex-kit::%advanced-indic-conjunct-break-class
             65 "OTHER")
            :to-equal "NONE")
    (dolist (case
             (list (list (code-char #x0301) "EXTEND" "EXTEND")
                   (list (code-char #x200c) "EXTEND" "NONE")
                   (list (code-char #x0915) "OTHER" "CONSONANT")
                   (list (code-char #x094d) "OTHER" "LINKER")))
      (destructuring-bind (character grapheme-class expected) case
        (expect (cl-regex-kit::%advanced-indic-conjunct-break-class
                 character
                 grapheme-class)
                :to-equal expected)))
    (let ((ctx (context "a")))
      (signals error
        (cl-regex-kit::%advanced-node-evaluate
         (make-instance 'cl-regex-kit::regex-node)
         (state 0 nil)
         ctx
         0)))

    (let ((ctx (context "a")))
      (expect (cl-regex-kit::%advanced-grapheme-boundary-p
               ctx -1 t)
              :to-be nil)
      (expect (cl-regex-kit::%advanced-grapheme-boundary-p
               ctx 0 nil)
              :to-be-truthy)
      (expect (cl-regex-kit::%advanced-grapheme-boundary-p
               ctx 1 t)
              :to-be-truthy)
      (expect (cl-regex-kit::%advanced-grapheme-boundary-p
               ctx 2 t)
              :to-be nil))
    (let ((utf8
            (context (make-array 2
                                 :element-type '(unsigned-byte 8)
                                 :initial-contents '(#xC3 #xA9))
                     :byte-mode-p t)))
      (expect (cl-regex-kit::%advanced-grapheme-boundary-p utf8 1 t)
              :to-be nil))

    (let ((regex (compile-regex "a")))
      (setf (slot-value regex 'cl-regex-kit::advanced-step-limit) 0)
      (signals error
        (cl-regex-kit:run-advanced-regex regex "a"))
      (setf (slot-value regex 'cl-regex-kit::advanced-step-limit) 1
            (slot-value regex 'cl-regex-kit::advanced-nest-limit) -1)
      (signals error
        (cl-regex-kit:run-advanced-regex regex "a"))))))
