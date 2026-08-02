;;;; t/pike-vm-test.lisp
(in-package #:cl-regex-kit/test)

(it
  "uses greedy and lazy split priority correctly"
  (expect (match-string (match "a+" "aaa") "aaa") :to-equal "aaa")
  (expect (match-string (match "a+?" "aaa") "aaa") :to-equal "a"))

(it
  "keeps captures isolated across split branches and priorities"
  (let ((result (match "(a)|b" "b")))
    (expect (match-string result "b") :to-equal "b")
    (expect (match-group-string result 1 "b") :to-be-null))
  (let ((result (match "(a|(b))" "b")))
    (expect (match-group-string result 1 "b") :to-equal "b")
    (expect (match-group-string result 2 "b") :to-equal "b"))
  (let ((greedy (match "(a+)(a)" "aaa"))
        (lazy (match "(a+?)(a)" "aaa")))
    (expect (match-group-string greedy 1 "aaa") :to-equal "aa")
    (expect (match-group-string greedy 2 "aaa") :to-equal "a")
    (expect (match-group-string lazy 1 "aaa") :to-equal "a")
    (expect (match-group-string lazy 2 "aaa") :to-equal "a")))

(it
  "applies dot terminators and validates VM execution bounds"
  (flet ((any-instruction (crlf-p line-terminator)
           (cl-regex-kit::make-inst
          :op
          :any
          :a
          (make-instance
            'cl-regex-kit::any-char-node
            :crlf-p
            crlf-p
            :line-terminator
            line-terminator))))
    (let ((crlf-dot (any-instruction t #\Newline))
          (custom-dot (any-instruction nil #\;)))
      (expect (cl-regex-kit::instruction-matches-p crlf-dot #\a) :to-be-truthy)
      (expect (cl-regex-kit::instruction-matches-p crlf-dot #\Return) :to-be-null)
      (expect (cl-regex-kit::instruction-matches-p crlf-dot #\Newline) :to-be-null)
      (expect (cl-regex-kit::instruction-matches-p crlf-dot #x0d) :to-be-null)
      (expect (cl-regex-kit::instruction-matches-p crlf-dot #x0a) :to-be-null)
      (expect (cl-regex-kit::instruction-matches-p custom-dot #\;) :to-be-null)
      (expect
        (cl-regex-kit::instruction-matches-p custom-dot #\Newline)
        :to-be-truthy)))
  (let ((program (cl-regex-kit::regex-program (compile-regex "a"))))
    (signals error (cl-regex-kit::run-pike-vm program "a" :start -1))
    (signals error (cl-regex-kit::run-pike-vm program "a" :start 1 :end 0))
    (signals
      error
      (cl-regex-kit::run-pike-vm program "a" :shortest-p t :longest-p t))))

(it-property
  "scan results always fall within the searched text bounds"
  ((text (gen-string :min-length 0 :max-length 40 :alphabet "ab"))
    (requested-start (gen-integer :min 0 :max 40)))
  (let* ((regex (compile-regex "a+|b+"))
         (start (min requested-start (length text)))
         (result (scan regex text :start start)))
    (when result
      (expect
        (<= 0 (match-start result) (match-end result) (length text))
        :to-be-truthy)
      (expect (>= (match-start result) start) :to-be-truthy))))

(it
  "validates set buffers, integer-PC closure, and byte word-half boundaries"
  (let* ((set (compile-regex-set (quote ("cat" "dog"))))
         (program (cl-regex-kit::regex-set-program set)))
    (signals
      type-error
      (cl-regex-kit::run-pike-vm-set program 2 "cat" :matches #*0))
    (signals
      type-error
      (cl-regex-kit::run-pike-vm-set program 2 "cat" :matches #(0 1)))
    (signals error (cl-regex-kit::run-pike-vm-set program 2 "cat" :start -1))
    (signals error (cl-regex-kit::run-pike-vm-set program 2 "cat" :start 1 :end 0))
    (signals error (cl-regex-kit::run-pike-vm-set program 2 "cat" :start 0.5))
    (signals error (cl-regex-kit::run-pike-vm-set program 2 "cat" :end 0.5))
    (let ((matches #*00))
      (expect
        (cl-regex-kit::run-pike-vm-set program 2 "cat" :matches matches)
        :to-be
        matches)
      (expect (coerce matches (quote list)) :to-equal (quote (1 0))))
    (expect (cl-regex-kit::run-pike-vm-set program 2 "cat")
            :to-equal
            (quote (0))))
  (let* ((set (compile-regex-set (quote ("a+" "a" "a+"))))
         (program (cl-regex-kit::regex-set-program set))
         (workspace (cl-regex-kit::make-pike-vm-closure-workspace (length program)))
         (closure
        (cl-regex-kit::pike-vm-set-closure
          program
          "aa"
          0
          2
          nil
          (list 0)
          :workspace
          workspace)))
    (expect (every (function integerp) closure) :to-be-truthy)
    (expect (regex-set-matches set "aa") :to-equal (quote (0 1 2)))
    (expect
      (cl-regex-kit::run-pike-vm-set program 3 "aa" :stop-at-first-match-p t)
      :to-be-truthy))
  (let ((text
        (make-array
          1
          :element-type
          (quote (unsigned-byte 8))
          :initial-contents
          (quote (97)))))
    (expect (cl-regex-kit::vm-word-position-p :end-half text 0 t nil) :to-be nil)
    (expect (cl-regex-kit::vm-word-position-p :end-half text 1 t nil) :to-be-truthy)))

(it
  "honors nonzero starts and explicit ends for captures and sets"
  (let* ((text "xxcatdog")
         (result (scan (compile-regex "(cat)") text :start 2 :end 5))
         (set (compile-regex-set (quote ("cat" "dog")))))
    (expect (match-start result) :to-equal 2)
    (expect (match-end result) :to-equal 5)
    (expect (match-group-start result 1) :to-equal 2)
    (expect (match-group-end result 1) :to-equal 5)
    (expect (match-group-string result 1 text) :to-equal "cat")
    (expect (regex-set-matches set text :start 2 :end 5) :to-equal (quote (0)))
    (expect (regex-set-match-p set text :start 2 :end 5) :to-be-truthy)))

(it
  "matches empty patterns when position equals the search limit"
  (let* ((text "abc")
         (result (scan (compile-regex "()") text :start 3 :end 3))
         (set (compile-regex-set (quote ("" "a")))))
    (expect (match-start result) :to-equal 3)
    (expect (match-end result) :to-equal 3)
    (expect (match-group-start result 1) :to-equal 3)
    (expect (match-group-end result 1) :to-equal 3)
    (expect (regex-set-matches set text :start 3 :end 3) :to-equal (quote (0)))
    (expect (regex-set-match-p set text :start 3 :end 3) :to-be-truthy)))

(it
  "preserves UTF-8 octet capture and set boundaries"
  (flet ((octets (&rest values)
           (make-array
          (length values)
          :element-type
          (quote (unsigned-byte 8))
          :initial-contents
          values)))
    (let* ((text (octets #xff #xc3 #xa9 #xfe))
           (result (scan (compile-byte-regex "(\\p{L})") text :start 1 :end 3))
           (set (compile-byte-regex-set (quote ("\\p{L}" "A")))))
      (expect (match-start result) :to-equal 1)
      (expect (match-end result) :to-equal 3)
      (expect (match-group-start result 1) :to-equal 1)
      (expect (match-group-end result 1) :to-equal 3)
      (expect
        (coerce (match-group-string result 1 text) (quote list))
        :to-equal
        (quote (#xc3 #xa9)))
      (expect (regex-set-matches set text :start 1 :end 3) :to-equal (quote (0)))
      (expect (regex-set-match-p set text :start 1 :end 3) :to-be-truthy))))

(progn
  (it
    "rejects mismatched reusable closure workspaces"
    (let* ((program (cl-regex-kit::regex-program (compile-regex "a")))
           (thread (cl-regex-kit::make-vm-thread :pc 0 :slots #()))
           (short-workspace
          (cl-regex-kit::make-pike-vm-closure-workspace (1- (length program)))))
      (signals
        error
        (cl-regex-kit::pike-vm-closure
          program
          "a"
          0
          1
          nil
          (list thread)
          :workspace
          short-workspace)))
    (let* ((set (compile-regex-set (quote ("a"))))
           (program (cl-regex-kit::regex-set-program set))
           (long-workspace
          (cl-regex-kit::make-pike-vm-closure-workspace (1+ (length program)))))
      (signals
        error
        (cl-regex-kit::pike-vm-set-closure
          program
          "a"
          0
          1
          nil
          (list 0)
          :workspace
          long-workspace))))
  (it
    "advances closure generations and clears marks only on wrap"
    (flet ((inst (op &key a b)
             (cl-regex-kit::make-inst :op op :a a :b b)))
      (let* ((program (vector (inst :jmp :a 1) (inst :match)))
             (workspace (cl-regex-kit::make-pike-vm-closure-workspace 2))
             (thread (cl-regex-kit::make-vm-thread :pc 0 :slots #())))
        (cl-regex-kit::pike-vm-closure
          program
          ""
          0
          0
          nil
          (list thread)
          :workspace
          workspace)
        (expect
          (cl-regex-kit::pike-vm-closure-workspace-generation workspace)
          :to-equal
          1)
        (cl-regex-kit::pike-vm-set-closure
          program
          ""
          0
          0
          nil
          (list 1)
          :workspace
          workspace)
        (expect
          (cl-regex-kit::pike-vm-closure-workspace-generation workspace)
          :to-equal
          2)
        (expect
          (coerce (cl-regex-kit::pike-vm-closure-workspace-marks workspace) (quote list))
          :to-equal
          (quote (1 2)))
        (setf (cl-regex-kit::pike-vm-closure-workspace-generation workspace) most-positive-fixnum)
        (fill
          (cl-regex-kit::pike-vm-closure-workspace-marks workspace)
          most-positive-fixnum)
        (expect
          (cl-regex-kit::pike-vm-set-closure
            program
            ""
            0
            0
            nil
            (list 1)
            :workspace
            workspace)
          :to-equal
          (quote (1)))
        (expect
          (cl-regex-kit::pike-vm-closure-workspace-generation workspace)
          :to-equal
          1)
        (expect
          (coerce (cl-regex-kit::pike-vm-closure-workspace-marks workspace) (quote list))
          :to-equal
          (quote (0 1)))))))

(it
  "preserves Pike closure priority and capture copy-on-write contracts"
  (flet ((inst (op &key a b)
           (cl-regex-kit::make-inst :op op :a a :b b)))
    (let* ((program
          (vector
            (inst :split :a 1 :b 3)
            (inst :save :a 0 :b 2)
            (inst :match)
            (inst :match)))
           (slots (vector nil nil))
           (capture
          (cl-regex-kit::pike-vm-closure
            program
            ""
            7
            7
            nil
            (list (cl-regex-kit::make-vm-thread :pc 0 :slots slots))))
           (set-closure (cl-regex-kit::pike-vm-set-closure program "" 7 7 nil (list 0))))
      (expect (mapcar (function cl-regex-kit::vm-thread-pc) capture) :to-equal (quote (2 3)))
      (expect set-closure :to-equal (quote (2 3)))
      (expect (eq (cl-regex-kit::vm-thread-slots (first capture)) slots) :to-be nil)
      (expect
        (eq (cl-regex-kit::vm-thread-slots (second capture)) slots)
        :to-be-truthy)
      (expect (aref slots 0) :to-be-null)
      (expect (aref (cl-regex-kit::vm-thread-slots (first capture)) 0) :to-equal 7))
    (let* ((program
          (vector
            (inst :split :a 1 :b 2)
            (inst :save :a 0 :b 3)
            (inst :jmp :a 3)
            (inst :match)))
           (slots (vector nil nil))
           (capture
          (cl-regex-kit::pike-vm-closure
            program
            ""
            7
            7
            nil
            (list (cl-regex-kit::make-vm-thread :pc 0 :slots slots))))
           (set-closure (cl-regex-kit::pike-vm-set-closure program "" 7 7 nil (list 0))))
      (expect (length capture) :to-equal 1)
      (expect (cl-regex-kit::vm-thread-pc (first capture)) :to-equal 3)
      (expect (aref (cl-regex-kit::vm-thread-slots (first capture)) 0) :to-equal 7)
      (expect set-closure :to-equal (quote (3))))))

(it
  "prioritizes pending paths and distinguishes shortest from longest"
  (let* ((regex (compile-regex "(a+)|(a)"))
         (result (scan regex "aa")))
    (expect (match-start result) :to-equal 0)
    (expect (match-end result) :to-equal 2)
    (expect (match-group-string result 1 "aa") :to-equal "aa")
    (expect (match-group-string result 2 "aa") :to-be-null))
  (let ((regex (compile-regex "a|aaa")))
    (expect (shortest-match regex "aaa") :to-equal 1)
    (expect (match-end (longest-match regex "aaa")) :to-equal 3)))

(it
  "evaluates end anchors against the absolute text end"
  (expect (scan (compile-regex "a$") "ab" :end 1) :to-be-null)
  (expect (scan (compile-regex "a\\z") "ab" :end 1) :to-be-null)
  (expect (match-end (scan (compile-regex "a$") "a" :end 1)) :to-equal 1)
  (expect (match-end (scan (compile-regex "a\\z") "a" :end 1)) :to-equal 1))

(it
  "handles empty, multiline, and bounded UTF-8 regex sets"
  (let ((set (compile-regex-set nil)))
    (expect (regex-set-count set) :to-equal 0)
    (expect (regex-set-matches set "anything") :to-equal nil)
    (expect (regex-set-match-p set "anything") :to-be nil))
  (let ((set (compile-regex-set (quote ("^b$" "^z$")) :multi-line t)))
    (expect (regex-set-matches set (format nil "a~%b~%c")) :to-equal (quote (0))))
  (flet ((octets (&rest bytes)
           (make-array
          (length bytes)
          :element-type
          (quote (unsigned-byte 8))
          :initial-contents
          bytes)))
    (let ((text (octets #xC3 #xA9 #x61))
          (set (compile-byte-regex-set (quote ("\\p{L}" "a")))))
      (expect (regex-set-matches set text :end 1) :to-equal nil)
      (expect (regex-set-matches set text :end 2) :to-equal (quote (0)))
      ;; Both members match: "a" (U+0061) is itself a Unicode Letter, so
      ;; \p{L} (pattern 0) matches it exactly as the literal "a" (pattern 1)
      ;; does -- byte-mode \p{L} decodes and classifies the UTF-8 scalar at
      ;; each position rather than treating multi-byte sequences specially.
      (expect (regex-set-matches set text :start 2 :end 3) :to-equal (quote (0 1))))))
