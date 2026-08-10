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
              :to-be-truthy))
    (let ((surrogate (code-char #xd800)))
      (when surrogate
        (signals type-error
          (cl-regex-kit::utf8-octets-for-character surrogate))))))

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

(it "classifies advanced AST features recursively"
  (flet ((literal ()
           (make-instance 'cl-regex-kit::literal-node :char #\a)))
    (dolist (node
             (list (make-instance 'cl-regex-kit::backreference-node)
                   (make-instance 'cl-regex-kit::grapheme-node)
                   (make-instance 'cl-regex-kit::assertion-node)
                   (make-instance 'cl-regex-kit::atomic-node :child (literal))
                   (make-instance 'cl-regex-kit::possessive-repetition-node
                                  :child (literal) :min 1 :max 1)
                   (make-instance 'cl-regex-kit::conditional-node)
                   (make-instance 'cl-regex-kit::subroutine-node)
                   (make-instance 'cl-regex-kit::control-verb-node :verb :fail)
                   (make-instance 'cl-regex-kit::callout-node :number 1)
                   (make-instance 'cl-regex-kit::reset-match-start-node)))
      (expect (cl-regex-kit::ast-contains-advanced-p node) :to-be-truthy))
    (dolist (kind '(:match-start :match-end :end-before-final-newline
                    :grapheme-boundary :word-boundary-unicode
                    :sentence-boundary))
      (expect (cl-regex-kit::ast-contains-advanced-p
               (make-instance 'cl-regex-kit::anchor-node :kind kind))
              :to-be-truthy))
    (dolist (node
             (list (literal)
                   (make-instance 'cl-regex-kit::char-class-node :ranges nil)
                   (make-instance 'cl-regex-kit::anchor-node :kind :line-start)
                   (make-instance 'cl-regex-kit::concat-node :children nil)
                   (make-instance 'cl-regex-kit::alternation-node :branches nil)))
      (expect (cl-regex-kit::ast-contains-advanced-p node) :to-be nil))
    (expect (cl-regex-kit::ast-contains-advanced-p
             (make-instance 'cl-regex-kit::concat-node
                            :children (list (literal)
                                            (make-instance 'cl-regex-kit::backreference-node))))
            :to-be-truthy)
    (expect (cl-regex-kit::ast-contains-advanced-p
             (make-instance 'cl-regex-kit::alternation-node
                            :branches (list (literal)
                                            (make-instance 'cl-regex-kit::grapheme-node))))
            :to-be-truthy)
    (expect (cl-regex-kit::ast-contains-advanced-p
             (make-instance 'cl-regex-kit::repetition-node
                            :child (make-instance 'cl-regex-kit::assertion-node)
                            :min 0))
            :to-be-truthy)
    (expect (cl-regex-kit::ast-contains-advanced-p
             (make-instance 'cl-regex-kit::group-node
                            :child (literal) :balance-name "stack"))
            :to-be-truthy)
    (expect (cl-regex-kit::ast-contains-advanced-p
             (make-instance 'cl-regex-kit::group-node :child (literal)))
            :to-be nil)))

(it "computes fixed widths for every fixed-length AST shape"
  (flet ((literal (character &key (raw-octet-p nil) (unicode-p t))
           (make-instance 'cl-regex-kit::literal-node
                          :char character
                          :raw-octet-p raw-octet-p
                          :unicode-p unicode-p))
         (width (node &optional (byte-mode-p t))
           (cl-regex-kit::ast-fixed-length node byte-mode-p)))
    (expect (width (literal #\a)) :to-equal 1)
    (expect (width (literal #\é)) :to-be nil)
    (expect (width (literal #\é :unicode-p nil)) :to-equal 1)
    (expect (width (literal #\é) nil) :to-equal 1)
    (expect (width (make-instance 'cl-regex-kit::char-class-node
                                  :ranges nil :unicode-p t))
            :to-be nil)
    (expect (width (make-instance 'cl-regex-kit::char-class-node
                                  :ranges nil :unicode-p nil))
            :to-equal 1)
    (expect (width (make-instance 'cl-regex-kit::any-char-node
                                  :unicode-p nil))
            :to-equal 1)
    (expect (width (make-instance 'cl-regex-kit::any-char-node
                                  :unicode-p t))
            :to-be nil)
    (expect (width (make-instance 'cl-regex-kit::any-char-node
                                  :crlf-p t))
            :to-be nil)
    (expect (width (make-instance 'cl-regex-kit::line-break-node)) :to-be nil)
    (expect (width (make-instance 'cl-regex-kit::anchor-node :kind :line-start))
            :to-equal 0)
    (expect (width (make-instance 'cl-regex-kit::reset-match-start-node))
            :to-equal 0)
    (expect (width (make-instance 'cl-regex-kit::assertion-node)) :to-equal 0)
    (expect (width (make-instance 'cl-regex-kit::concat-node :children nil))
            :to-equal 0)
    (expect (width (make-instance 'cl-regex-kit::concat-node
                                  :children (list (literal #\a) (literal #\b))))
            :to-equal 2)
    (expect (width (make-instance 'cl-regex-kit::concat-node
                                  :children (list (literal #\a)
                                                  (make-instance 'cl-regex-kit::line-break-node))))
            :to-be nil)
    (expect (width (make-instance 'cl-regex-kit::alternation-node :branches nil))
            :to-be nil)
    (expect (width (make-instance 'cl-regex-kit::alternation-node
                                  :branches (list (literal #\a) (literal #\b))))
            :to-equal 1)
    (expect (width (make-instance 'cl-regex-kit::alternation-node
                                  :branches (list (literal #\a)
                                                  (make-instance 'cl-regex-kit::concat-node
                                                                 :children (list (literal #\a)
                                                                                 (literal #\b))))))
            :to-be nil)
    (expect (width (make-instance 'cl-regex-kit::repetition-node
                                  :child (literal #\a) :min 2 :max 2))
            :to-equal 2)
    (expect (width (make-instance 'cl-regex-kit::repetition-node
                                  :child (literal #\a) :min 1 :max nil))
            :to-be nil)
    (expect (width (make-instance 'cl-regex-kit::repetition-node
                                  :child (literal #\a) :min 1 :max 2))
            :to-be nil)
    (expect (width (make-instance 'cl-regex-kit::repetition-node
                                  :child (make-instance 'cl-regex-kit::line-break-node)
                                  :min 1 :max 1))
            :to-be nil)
    (expect (width (make-instance 'cl-regex-kit::possessive-repetition-node
                                  :child (literal #\a) :min 3 :max 3))
            :to-equal 3)
    (expect (width (make-instance 'cl-regex-kit::possessive-repetition-node
                                  :child (literal #\a) :min 1 :max nil))
            :to-be nil)
    (expect (width (make-instance 'cl-regex-kit::group-node
                                  :child (literal #\a))) :to-equal 1)
    (expect (width (make-instance 'cl-regex-kit::atomic-node
                                  :child (literal #\a))) :to-equal 1)))

(it "normalizes advanced AST containers without losing their metadata"
  (let* ((unicode (make-instance 'cl-regex-kit::literal-node :char #\é))
         (possessive (make-instance 'cl-regex-kit::possessive-repetition-node
                                    :child unicode :min 1 :max 2
                                    :greedy-p nil :possessive-p t))
         (assertion (make-instance 'cl-regex-kit::lookaround-node
                                   :kind :lookbehind :child unicode
                                   :negative-p t :direction :backward
                                   :fixed-length 1 :non-atomic-p t))
         (atomic (make-instance 'cl-regex-kit::atomic-node :child unicode))
         (conditional (make-instance 'cl-regex-kit::conditional-node
                                     :condition :capture
                                     :yes-branch unicode
                                     :no-branch (make-instance 'cl-regex-kit::literal-node
                                                               :char #\a)))
         (recursive (make-instance 'cl-regex-kit::recursion-node
                                   :target unicode :name "letter"
                                   :capture-index 4))
         (normalized-possessive
           (cl-regex-kit::normalize-byte-literals possessive))
         (normalized-assertion
           (cl-regex-kit::normalize-byte-literals assertion))
         (normalized-atomic
           (cl-regex-kit::normalize-byte-literals atomic))
         (normalized-conditional
           (cl-regex-kit::normalize-byte-literals conditional))
         (normalized-recursive
           (cl-regex-kit::normalize-byte-literals recursive))
         (symbolic (make-instance 'cl-regex-kit::subroutine-node
                                  :target "letter")))
    (expect (typep normalized-possessive
                   'cl-regex-kit::possessive-repetition-node)
            :to-be-truthy)
    (expect (cl-regex-kit::possessive-repetition-node-min normalized-possessive)
            :to-equal 1)
    (expect (cl-regex-kit::possessive-repetition-node-max normalized-possessive)
            :to-equal 2)
    (expect (cl-regex-kit::possessive-repetition-node-greedy-p normalized-possessive)
            :to-be nil)
    (expect (cl-regex-kit::possessive-repetition-node-possessive-p normalized-possessive)
            :to-be-truthy)
    (expect (typep normalized-assertion 'cl-regex-kit::lookaround-node)
            :to-be-truthy)
    (expect (cl-regex-kit::assertion-node-negative-p normalized-assertion)
            :to-be-truthy)
    (expect (cl-regex-kit::assertion-node-direction normalized-assertion)
            :to-be :backward)
    (expect (cl-regex-kit::assertion-node-fixed-length normalized-assertion)
            :to-equal 1)
    (expect (cl-regex-kit::assertion-node-non-atomic-p normalized-assertion)
            :to-be-truthy)
    (expect (typep normalized-atomic 'cl-regex-kit::atomic-node) :to-be-truthy)
    (expect (typep (cl-regex-kit::atomic-node-child normalized-atomic)
                   'cl-regex-kit::concat-node)
            :to-be-truthy)
    (expect (cl-regex-kit::conditional-node-condition normalized-conditional)
            :to-be :capture)
    (expect (typep (cl-regex-kit::conditional-node-yes-branch normalized-conditional)
                   'cl-regex-kit::concat-node)
            :to-be-truthy)
    (expect (typep normalized-recursive 'cl-regex-kit::recursion-node)
            :to-be-truthy)
    (expect (cl-regex-kit::subroutine-node-name normalized-recursive)
            :to-equal "letter")
    (expect (cl-regex-kit::subroutine-node-capture-index normalized-recursive)
            :to-equal 4)
    (expect (cl-regex-kit::normalize-byte-literals symbolic) :to-be symbolic)
    (let ((empty (make-instance 'cl-regex-kit::assertion-node)))
      (expect (cl-regex-kit::normalize-byte-literals empty) :to-be empty))))

(it "annotates lookbehind lengths through recursive AST data"
  (let* ((fixed (make-instance 'cl-regex-kit::concat-node
                               :children (list
                                          (make-instance 'cl-regex-kit::literal-node
                                                         :char #\a)
                                          (make-instance 'cl-regex-kit::literal-node
                                                         :char #\b))))
         (backward (make-instance 'cl-regex-kit::assertion-node
                                  :direction :backward :child fixed))
         (forward (make-instance 'cl-regex-kit::assertion-node
                                 :direction :forward :child fixed))
         (unknown (make-instance 'cl-regex-kit::assertion-node
                                 :direction :backward
                                 :child (make-instance 'cl-regex-kit::line-break-node)))
         (conditional (make-instance 'cl-regex-kit::conditional-node
                                     :condition fixed
                                     :yes-branch backward
                                     :no-branch unknown))
         (symbolic (make-instance 'cl-regex-kit::subroutine-node
                                  :target "letter"))
         (resolved (make-instance 'cl-regex-kit::subroutine-node
                                  :target fixed))
         (cycle (make-instance 'cl-regex-kit::concat-node :children nil))
         (root nil))
    (setf (slot-value cycle 'cl-regex-kit::children) (list cycle))
    (setf root (make-instance 'cl-regex-kit::concat-node
                              :children (list backward forward unknown
                                              conditional symbolic resolved cycle)))
    (expect (cl-regex-kit::annotate-lookbehind-lengths root t) :to-be root)
    (expect (cl-regex-kit::assertion-node-fixed-length backward) :to-equal 2)
    (expect (cl-regex-kit::assertion-node-fixed-length forward) :to-be nil)
    (expect (cl-regex-kit::assertion-node-fixed-length unknown) :to-be nil)))

(it "finds the highest capture index across AST containers"
  (flet ((capture (index)
           (make-instance 'cl-regex-kit::group-node
                          :child (make-instance 'cl-regex-kit::literal-node
                                                :char #\a)
                          :capture-index index)))
    (dolist (case
             (list
              (cons (make-instance 'cl-regex-kit::literal-node :char #\a) 0)
              (cons (capture 3) 3)
              (cons (make-instance 'cl-regex-kit::concat-node
                                   :children (list (capture 2) (capture 7))) 7)
              (cons (make-instance 'cl-regex-kit::alternation-node
                                   :branches (list (capture 4) (capture 6))) 6)
              (cons (make-instance 'cl-regex-kit::repetition-node
                                   :child (capture 5) :min 0) 5)
              (cons (make-instance 'cl-regex-kit::possessive-repetition-node
                                   :child (capture 8) :min 1) 8)
              (cons (make-instance 'cl-regex-kit::assertion-node
                                   :child (capture 9)) 9)
              (cons (make-instance 'cl-regex-kit::atomic-node
                                   :child (capture 10)) 10)
              (cons (make-instance 'cl-regex-kit::conditional-node
                                   :yes-branch (capture 11)
                                   :no-branch (capture 12)) 12)
              (cons (make-instance 'cl-regex-kit::subroutine-node
                                   :target (capture 13)) 13)
              (cons (make-instance 'cl-regex-kit::subroutine-node
                                   :target "letter") 0)))
      (expect (cl-regex-kit::ast-group-count (car case)) :to-equal (cdr case)))))
