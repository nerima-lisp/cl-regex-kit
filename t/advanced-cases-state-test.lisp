(in-package #:cl-regex-kit/test)

(it "covers advanced state, capture, and byte-element contracts"
  (with-advanced-state-fixtures (context state group)
    (expect (cl-regex-kit::%advanced-name= "Word" "word") :to-be-truthy)
    (expect (cl-regex-kit::%advanced-name= nil "word") :to-be nil)
    (expect (cl-regex-kit::%advanced-element-equal-p #\A #\a t nil)
            :to-be-truthy)
    (expect (cl-regex-kit::%advanced-element-equal-p #\É #\é t t)
            :to-be-truthy)
    (expect (cl-regex-kit::%advanced-element-equal-p 65 97 t nil)
            :to-be-truthy)
    (expect (cl-regex-kit::%advanced-element-equal-p 65 66 nil nil)
            :to-be nil)
    (expect (cl-regex-kit::%advanced-element-equal-p #\a 97 nil nil)
            :to-be nil)
    (let* ((first-group (group "first" 1))
           (second-group (group "second" 2))
           (wrapped (list
                     first-group
                     (make-instance 'cl-regex-kit::concat-node
                                    :children (list second-group))
                     (make-instance 'cl-regex-kit::alternation-node
                                    :branches (list first-group second-group))
                     (make-instance 'cl-regex-kit::repetition-node
                                    :child first-group :min 0 :max 1)
                     (make-instance 'cl-regex-kit::possessive-repetition-node
                                    :child second-group :min 1 :max 2)
                     (make-instance 'cl-regex-kit::assertion-node
                                    :child first-group)
                     (make-instance 'cl-regex-kit::atomic-node
                                    :child second-group)
                     (make-instance 'cl-regex-kit::conditional-node
                                    :yes-branch first-group
                                    :no-branch second-group)))
           (root (make-instance 'cl-regex-kit::concat-node :children wrapped))
           (ctx (context "a" :root root)))
      (dolist (name '("first" "second"))
        (expect (cl-regex-kit::%advanced-find-group root :name name)
                :to-be-truthy))
      (expect (cl-regex-kit::%advanced-find-group root :index 2)
              :to-be second-group)
      (expect (cl-regex-kit::%advanced-find-group
               (make-instance 'cl-regex-kit::literal-node :char #\b)
               :name "missing")
              :to-be nil)
      (expect (cl-regex-kit::%advanced-capture-index-for-group nil)
              :to-be nil)
      (let ((backreference
              (make-instance 'cl-regex-kit::backreference-node
                             :name "SECOND")))
        (expect (cl-regex-kit::%advanced-capture-index backreference ctx)
                :to-equal 2)))
    (let* ((slots (make-array 5 :initial-element nil))
           (ctx (context "abc"
                         :group-names (list (cons "word" 0)
                                            (cons "WORD" 1))))
           (unparticipated (state 0 (make-array 4 :initial-element nil)))
           (participating (state 0 (make-array 4
                                               :initial-contents
                                               '(nil nil 2 3)))))
      (cl-regex-kit::%advanced-initialize-capture-stacks slots 1)
      (setf (aref (aref slots 4) 0) (list (cons 5 6))
            (aref (aref slots 4) 1) (list (cons 7 8)))
      (let ((capture-state (state 0 slots)))
        (cl-regex-kit::%advanced-sync-capture-slot capture-state 0)
        (expect (cl-regex-kit::%advanced-capture-stacks capture-state)
                :to-be-truthy)
        (expect (cl-regex-kit::%advanced-capture-participated-p 0 capture-state)
                :to-be-truthy)
        (expect (cl-regex-kit::%advanced-capture-participated-p 9 capture-state)
                :to-be nil)
        (let ((copy (cl-regex-kit::%advanced-copy-slots slots)))
          (setf (car (first (aref (aref copy 4) 0))) 99)
          (expect (car (first (aref (aref slots 4) 0))) :to-equal 5)))
      (expect (cl-regex-kit::%advanced-capture-index-by-name
               "word" ctx)
              :to-equal 0)
      (expect (cl-regex-kit::%advanced-capture-index-by-name
               "word" ctx participating)
              :to-equal 1)
      (expect (cl-regex-kit::%advanced-capture-index-by-name
               "word" ctx unparticipated)
              :to-equal 0)
      (expect (cl-regex-kit::%advanced-capture-index-by-name
               "missing" ctx)
              :to-be nil)
      (let ((source (state 1 slots :mark "mark")))
        (let ((copy (cl-regex-kit::%advanced-state-copy source)))
          (expect (cl-regex-kit::advanced-state-position copy) :to-equal 1)
          (setf (cl-regex-kit::advanced-state-mark copy) "copy")
          (expect (cl-regex-kit::advanced-state-mark source)
                  :to-equal "mark"))))
    (let ((string-context (context "a"))
          (byte-context
            (context (make-array 2
                                 :element-type '(unsigned-byte 8)
                                 :initial-contents '(#xC3 #xA9))
                     :byte-mode-p t))
          (invalid-context
            (context (make-array 1
                                 :element-type '(unsigned-byte 8)
                                 :initial-contents '(#xFF))
                     :byte-mode-p t))
          (octet-context
            (context (make-array 1
                                 :element-type '(unsigned-byte 8)
                                 :initial-contents '(65))
                     :byte-mode-p t)))
      (multiple-value-bind (value end valid-p)
          (cl-regex-kit::%advanced-read-element string-context 0 t)
        (expect value :to-be #\a)
        (expect end :to-equal 1)
        (expect valid-p :to-be-truthy))
      (multiple-value-bind (value end valid-p)
          (cl-regex-kit::%advanced-read-element string-context 1 t)
        (expect value :to-be nil)
        (expect end :to-be nil)
        (expect valid-p :to-be nil))
      (multiple-value-bind (value end valid-p)
          (cl-regex-kit::%advanced-read-element byte-context 0 t)
        (expect value :to-be #\é)
        (expect end :to-equal 2)
        (expect valid-p :to-be-truthy))
      (multiple-value-bind (value end valid-p)
          (cl-regex-kit::%advanced-read-element invalid-context 0 t)
        (expect value :to-be nil)
        (expect end :to-be nil)
        (expect valid-p :to-be nil))
      (multiple-value-bind (value end valid-p)
          (cl-regex-kit::%advanced-read-element octet-context 0 nil)
        (expect value :to-equal 65)
        (expect end :to-equal 1)
        (expect valid-p :to-be-truthy)))))
