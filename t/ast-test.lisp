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

(it "normalizes non-ASCII byte literals into their UTF-8 octets"
  (let* ((literal (make-instance 'cl-regex-kit::literal-node
                                 :char #\é :unicode-p nil))
         (normalized (cl-regex-kit::normalize-byte-literals literal)))
    (expect (typep normalized 'cl-regex-kit::concat-node) :to-be-truthy)
    (expect (mapcar (lambda (node)
                      (char-code (cl-regex-kit::literal-node-char node)))
                    (cl-regex-kit::concat-node-children normalized))
            :to-equal '(#xc3 #xa9))))

(it "preserves raw byte literals and reconstructs changed AST containers"
  (let* ((raw (make-instance 'cl-regex-kit::literal-node
                             :char (code-char #xe9)
                             :raw-octet-p t :unicode-p nil))
         (literal (make-instance 'cl-regex-kit::literal-node
                                 :char #\é :unicode-p nil))
         (group (make-instance 'cl-regex-kit::group-node
                               :child literal :capture-index 3 :name "letter"))
         (repetition (make-instance 'cl-regex-kit::repetition-node
                                    :child group :min 1 :max 2 :greedy-p nil))
         (normalized (cl-regex-kit::normalize-byte-literals repetition)))
    (expect (cl-regex-kit::normalize-byte-literals raw) :to-be raw)
    (expect (cl-regex-kit::repetition-node-min normalized) :to-equal 1)
    (expect (cl-regex-kit::repetition-node-max normalized) :to-equal 2)
    (expect (cl-regex-kit::repetition-node-greedy-p normalized) :to-be nil)
    (let ((normalized-group (cl-regex-kit::repetition-node-child normalized)))
      (expect (cl-regex-kit::group-node-capture-index normalized-group) :to-equal 3)
      (expect (cl-regex-kit::group-node-name normalized-group) :to-equal "letter"))))

(it "computes static capture counts only when every successful match agrees"
  (flet ((capture ()
           (make-instance 'cl-regex-kit::group-node
                          :child (make-instance 'cl-regex-kit::literal-node :char #\a)
                          :capture-index 1))
         (count-of (node)
           (multiple-value-list (cl-regex-kit::ast-static-capture-count node))))
    (expect (count-of (make-instance 'cl-regex-kit::concat-node
                                     :children (list (capture) (capture))))
            :to-equal '(2 t))
    (expect (count-of (make-instance 'cl-regex-kit::alternation-node
                                     :branches (list (capture) (capture))))
            :to-equal '(1 t))
    (expect (count-of (make-instance 'cl-regex-kit::alternation-node
                                     :branches (list (capture)
                                                    (make-instance 'cl-regex-kit::literal-node :char #\a))))
            :to-equal '(nil nil))
    (expect (count-of (make-instance 'cl-regex-kit::repetition-node
                                     :child (capture) :min 0 :max 1))
            :to-equal '(nil nil))
      (expect (count-of (make-instance 'cl-regex-kit::repetition-node
                                       :child (capture) :min 2 :max 2))
              :to-equal '(1 t))
    (expect (count-of (make-instance 'cl-regex-kit::alternation-node
                                     :branches (list (make-instance 'cl-regex-kit::alternation-node
                                                                    :branches (list (capture)
                                                                                   (make-instance 'cl-regex-kit::literal-node :char #\a)))
                                                    (capture))))
            :to-equal '(nil nil))))

(it "encodes every UTF-8 scalar width and only rebuilds changed containers"
  (flet ((literal (character &key (unicode-p nil))
           (make-instance 'cl-regex-kit::literal-node
                          :char character :unicode-p unicode-p)))
    (dolist (case `((#\A (65))
                    (,(code-char #x00e9) (195 169))
                    (,(code-char #x2603) (226 152 131))
                    (,(code-char #x1f600) (240 159 152 128))))
      (destructuring-bind (character expected-octets) case
        (expect (cl-regex-kit::utf8-octets-for-character character)
                :to-equal expected-octets)))
    (let* ((ascii (literal #\a))
           (unchanged-concat (make-instance 'cl-regex-kit::concat-node
                                            :children (list ascii)))
           (wide (literal (code-char #x2603)))
           (concat (make-instance 'cl-regex-kit::concat-node
                                  :children (list ascii wide)))
           (alternation (make-instance 'cl-regex-kit::alternation-node
                                       :branches (list ascii wide))))
      (expect (cl-regex-kit::normalize-byte-literals ascii) :to-be ascii)
      (expect (cl-regex-kit::normalize-byte-literals unchanged-concat)
              :to-be unchanged-concat)
      (expect (typep (cl-regex-kit::normalize-byte-literals concat)
                     'cl-regex-kit::concat-node)
              :to-be-truthy)
      (expect (typep (cl-regex-kit::normalize-byte-literals alternation)
                     'cl-regex-kit::alternation-node)
              :to-be-truthy))))

(it-property
  "UTF-8 encoding round-trips for every generated Unicode scalar"
  ((character (gen-map #'code-char
                       (gen-such-that (lambda (code) (not (<= #xd800 code #xdfff)))
                                      (gen-integer :min 0 :max #x10ffff))
                       :name :unicode-scalar)))
  (let* ((octets (coerce (cl-regex-kit::utf8-octets-for-character character) 'vector))
         (decoded (cl-regex-kit::utf8-character-at octets 0)))
    (expect decoded :to-be character)))

(it "recognizes capture-stable empty and non-empty repetitions"
  (labels ((literal ()
           (make-instance 'cl-regex-kit::literal-node :char #\a))
         (capture ()
           (make-instance 'cl-regex-kit::group-node
                          :child (literal) :capture-index 1))
         (count-of (node)
           (multiple-value-list (cl-regex-kit::ast-static-capture-count node))))
    (expect (count-of (make-instance 'cl-regex-kit::repetition-node
                                     :child (capture) :min 0 :max 0))
            :to-equal '(0 t))
    (expect (count-of (make-instance 'cl-regex-kit::repetition-node
                                     :child (literal) :min 0 :max nil))
            :to-equal '(0 t))
    (expect (count-of (make-instance 'cl-regex-kit::repetition-node
                                     :child (literal) :min 1 :max nil))
            :to-equal '(0 t))
    (expect (count-of (make-instance 'cl-regex-kit::group-node
                                     :child (literal)))
            :to-equal '(0 t))))
