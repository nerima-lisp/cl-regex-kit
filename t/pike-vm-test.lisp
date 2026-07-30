;;;; t/pike-vm-test.lisp
(in-package #:cl-regex-kit/test)

(it "uses greedy and lazy split priority correctly"
  (expect (match-string (match "a+" "aaa") "aaa") :to-equal "aaa")
  (expect (match-string (match "a+?" "aaa") "aaa") :to-equal "a"))

(it "keeps captures for the selected leftmost-first path"
  (let ((result (match "(a+)(b)" "xaaab")))
    (expect (match-start result) :to-equal 1)
    (expect (match-group-string result 1 "xaaab") :to-equal "aaa")
    (expect (match-group-string result 2 "xaaab") :to-equal "b")))

(it "applies dot terminators and validates VM execution bounds"
  (flet ((any-instruction (crlf-p line-terminator)
           (cl-regex-kit::make-inst
            :op :any
            :a (make-instance 'cl-regex-kit::any-char-node
                              :crlf-p crlf-p
                              :line-terminator line-terminator))))
    (let ((crlf-dot (any-instruction t #\Newline))
          (custom-dot (any-instruction nil #\;)))
      (expect (cl-regex-kit::instruction-matches-p crlf-dot #\a) :to-be-truthy)
      (expect (cl-regex-kit::instruction-matches-p crlf-dot #\Return) :to-be-null)
      (expect (cl-regex-kit::instruction-matches-p crlf-dot #\Newline) :to-be-null)
      (expect (cl-regex-kit::instruction-matches-p crlf-dot #x0d) :to-be-null)
      (expect (cl-regex-kit::instruction-matches-p crlf-dot #x0a) :to-be-null)
      (expect (cl-regex-kit::instruction-matches-p custom-dot #\;) :to-be-null)
      (expect (cl-regex-kit::instruction-matches-p custom-dot #\Newline)
              :to-be-truthy)))
  (let ((program (cl-regex-kit::regex-program (compile-regex "a"))))
    (signals error (cl-regex-kit::run-pike-vm program "a" :start -1))
    (signals error (cl-regex-kit::run-pike-vm program "a" :start 1 :end 0))
    (signals error (cl-regex-kit::run-pike-vm program "a" :shortest-p t :longest-p t))))

(it "validates set buffers and byte word-half boundaries in the VM"
  (let* ((set (compile-regex-set '("cat" "dog")))
         (program (cl-regex-kit::regex-set-program set)))
    (signals type-error
      (cl-regex-kit::run-pike-vm-set program 2 "cat" :matches #*0))
    (signals type-error
      (cl-regex-kit::run-pike-vm-set program 2 "cat" :matches #(0 1)))
    (let ((matches #*00))
      (expect (cl-regex-kit::run-pike-vm-set program 2 "cat" :matches matches)
              :to-be matches)
      (expect (coerce matches 'list) :to-equal '(1 0))))
  (let ((text (make-array 1
                          :element-type '(unsigned-byte 8)
                          :initial-contents '(97))))
    (expect (cl-regex-kit::vm-word-position-p :end-half text 0 t nil)
            :to-be nil)
    (expect (cl-regex-kit::vm-word-position-p :end-half text 1 t nil)
            :to-be-truthy)))
