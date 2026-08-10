(in-package #:cl-regex-kit/test)

(it "normalizes every byte-mode AST container while preserving node metadata"
  (let* ((wide (make-instance 'cl-regex-kit::literal-node
                              :char #\é :unicode-p t))
         (raw (make-instance 'cl-regex-kit::literal-node
                             :char (code-char #xe9)
                             :raw-octet-p t
                             :unicode-p nil))
         (repetition (make-instance 'cl-regex-kit::repetition-node
                                    :child raw :min 0 :max 1
                                    :greedy-p t))
         (char-class (make-instance 'cl-regex-kit::char-class-node
                                    :ranges nil))
         (any-char (make-instance 'cl-regex-kit::any-char-node))
         (anchor (make-instance 'cl-regex-kit::anchor-node :kind :line-start))
         (line-break (make-instance 'cl-regex-kit::line-break-node))
         (reset (make-instance 'cl-regex-kit::reset-match-start-node))
         (possessive (make-instance 'cl-regex-kit::possessive-repetition-node
                                    :child wide :min 1 :max 2
                                    :greedy-p nil :possessive-p t))
         (identity-possessive
           (make-instance 'cl-regex-kit::possessive-repetition-node
                          :child raw :min 1 :max 2
                          :greedy-p t :possessive-p t))
         (lookaround (make-instance 'cl-regex-kit::lookaround-node
                                    :kind :lookbehind
                                    :child wide
                                    :direction :backward
                                    :fixed-length 4
                                    :non-atomic-p t))
         (atomic (make-instance 'cl-regex-kit::atomic-node :child wide))
         (conditional (make-instance 'cl-regex-kit::conditional-node
                                     :condition :capture
                                     :yes-branch wide
                                     :no-branch raw))
         (identity-conditional
           (make-instance 'cl-regex-kit::conditional-node
                          :condition :capture
                          :yes-branch raw
                          :no-branch raw))
         (recursion (make-instance 'cl-regex-kit::recursion-node
                                   :target wide
                                   :name "body"
                                   :capture-index 4
                                   :recursive-p t))
         (identity-recursion
           (make-instance 'cl-regex-kit::recursion-node
                          :target raw
                          :name "raw"
                          :capture-index 5
                          :recursive-p nil)))
    (dolist (node (list raw char-class any-char anchor line-break reset))
      (expect (cl-regex-kit::normalize-byte-literals node) :to-be node))
    (dolist (node (list repetition identity-possessive
                         identity-conditional identity-recursion))
      (expect (cl-regex-kit::normalize-byte-literals node) :to-be node))
    (expect (cl-regex-kit::normalize-byte-literals wide nil) :to-be wide)
    (let ((normalized
            (cl-regex-kit::normalize-byte-literals possessive)))
      (expect (typep normalized 'cl-regex-kit::possessive-repetition-node)
              :to-be-truthy)
      (expect (cl-regex-kit::possessive-repetition-node-min normalized)
              :to-equal 1)
      (expect (cl-regex-kit::possessive-repetition-node-max normalized)
              :to-equal 2)
      (expect (cl-regex-kit::possessive-repetition-node-greedy-p normalized)
              :to-be-falsy)
      (expect (cl-regex-kit::possessive-repetition-node-possessive-p normalized)
              :to-be-truthy))
    (let ((normalized
            (cl-regex-kit::normalize-byte-literals lookaround)))
      (expect (typep normalized 'cl-regex-kit::lookaround-node)
              :to-be-truthy)
      (expect (cl-regex-kit::assertion-node-kind normalized)
              :to-be :lookbehind)
      (expect (cl-regex-kit::assertion-node-direction normalized)
              :to-be :backward)
      (expect (cl-regex-kit::assertion-node-fixed-length normalized)
              :to-equal 4)
      (expect (cl-regex-kit::assertion-node-non-atomic-p normalized)
              :to-be-truthy))
    (dolist (node (list atomic conditional recursion))
      (expect (cl-regex-kit::normalize-byte-literals node)
              :to-be-truthy))
    (let ((normalized
            (cl-regex-kit::normalize-byte-literals recursion)))
      (expect (typep normalized 'cl-regex-kit::recursion-node)
              :to-be-truthy)
      (expect (cl-regex-kit::subroutine-node-name normalized)
              :to-equal "body")
      (expect (cl-regex-kit::subroutine-node-capture-index normalized)
              :to-equal 4)
      (expect (cl-regex-kit::subroutine-node-recursive-p normalized)
              :to-be-truthy))))

(it "classifies advanced nodes and computes exact AST widths"
  (let* ((ascii (make-instance 'cl-regex-kit::literal-node :char #\a))
         (wide (make-instance 'cl-regex-kit::literal-node :char #\é))
         (unknown (make-instance 'cl-regex-kit::regex-node))
         (two (make-instance 'cl-regex-kit::concat-node
                             :children (list ascii ascii)))
         (captured (make-instance 'cl-regex-kit::group-node
                                  :child ascii :capture-index 5)))
    (expect (cl-regex-kit::ast-contains-advanced-p ascii) :to-be-falsy)
    (expect (cl-regex-kit::ast-contains-advanced-p
             (make-instance 'cl-regex-kit::anchor-node :kind :line-start))
            :to-be-falsy)
    (expect (cl-regex-kit::ast-contains-advanced-p
             (make-instance 'cl-regex-kit::anchor-node :kind :match-end))
            :to-be-truthy)
    (dolist (node (list
                   (make-instance 'cl-regex-kit::backreference-node)
                   (make-instance 'cl-regex-kit::grapheme-node)
                   (make-instance 'cl-regex-kit::assertion-node)
                   (make-instance 'cl-regex-kit::atomic-node :child ascii)
                   (make-instance 'cl-regex-kit::possessive-repetition-node
                                  :child ascii :min 1 :max 1)
                   (make-instance 'cl-regex-kit::conditional-node)
                   (make-instance 'cl-regex-kit::subroutine-node)
                   (make-instance 'cl-regex-kit::control-verb-node :verb :fail)
                   (make-instance 'cl-regex-kit::callout-node)
                   (make-instance 'cl-regex-kit::reset-match-start-node)))
      (expect (cl-regex-kit::ast-contains-advanced-p node) :to-be-truthy))
    (expect (cl-regex-kit::ast-contains-advanced-p
             (make-instance 'cl-regex-kit::concat-node :children (list ascii
                                                                        (make-instance 'cl-regex-kit::anchor-node
                                                                                       :kind :match-start))))
            :to-be-truthy)
    (expect (cl-regex-kit::ast-contains-advanced-p
             (make-instance 'cl-regex-kit::alternation-node
                            :branches (list ascii
                                             (make-instance 'cl-regex-kit::grapheme-node))))
            :to-be-truthy)
    (expect (cl-regex-kit::ast-contains-advanced-p
             (make-instance 'cl-regex-kit::repetition-node
                            :child (make-instance 'cl-regex-kit::grapheme-node)
                            :min 0 :max 1))
            :to-be-truthy)
    (expect (cl-regex-kit::ast-contains-advanced-p
             (make-instance 'cl-regex-kit::group-node
                            :child ascii :balance-name "stack"))
            :to-be-truthy)
    (expect (cl-regex-kit::ast-fixed-length ascii t) :to-equal 1)
    (expect (cl-regex-kit::ast-fixed-length wide t) :to-be-null)
    (expect (cl-regex-kit::ast-fixed-length wide nil) :to-equal 1)
    (expect (cl-regex-kit::ast-fixed-length
             (make-instance 'cl-regex-kit::char-class-node
                            :ranges nil :unicode-p t) t)
            :to-be-null)
    (expect (cl-regex-kit::ast-fixed-length
             (make-instance 'cl-regex-kit::char-class-node
                            :ranges nil :unicode-p nil) t)
            :to-equal 1)
    (expect (cl-regex-kit::ast-fixed-length
             (make-instance 'cl-regex-kit::any-char-node :crlf-p t) nil)
            :to-be-null)
    (expect (cl-regex-kit::ast-fixed-length
             (make-instance 'cl-regex-kit::any-char-node :unicode-p t) t)
            :to-be-null)
    (expect (cl-regex-kit::ast-fixed-length
             (make-instance 'cl-regex-kit::any-char-node :unicode-p nil) t)
            :to-equal 1)
    (expect (cl-regex-kit::ast-fixed-length
             (make-instance 'cl-regex-kit::line-break-node) t) :to-be-null)
    (dolist (node (list
                   (make-instance 'cl-regex-kit::anchor-node :kind :line-start)
                   (make-instance 'cl-regex-kit::reset-match-start-node)
                   (make-instance 'cl-regex-kit::assertion-node :child wide)))
      (expect (cl-regex-kit::ast-fixed-length node t) :to-equal 0))
    (expect (cl-regex-kit::ast-fixed-length two t) :to-equal 2)
    (expect (cl-regex-kit::ast-fixed-length
             (make-instance 'cl-regex-kit::concat-node
                            :children (list ascii unknown)) t)
            :to-be-null)
    (expect (cl-regex-kit::ast-fixed-length
             (make-instance 'cl-regex-kit::alternation-node
                            :branches nil) t)
            :to-be-null)
    (expect (cl-regex-kit::ast-fixed-length
             (make-instance 'cl-regex-kit::alternation-node
                            :branches (list ascii ascii)) t)
            :to-equal 1)
    (expect (cl-regex-kit::ast-fixed-length
             (make-instance 'cl-regex-kit::alternation-node
                            :branches (list ascii two)) t)
            :to-be-null)
    (expect (cl-regex-kit::ast-fixed-length
             (make-instance 'cl-regex-kit::repetition-node
                            :child ascii :min 2 :max 2) t)
            :to-equal 2)
    (expect (cl-regex-kit::ast-fixed-length
             (make-instance 'cl-regex-kit::repetition-node
                            :child ascii :min 0) t)
            :to-be-null)
    (expect (cl-regex-kit::ast-fixed-length
             (make-instance 'cl-regex-kit::possessive-repetition-node
                            :child ascii :min 2 :max 2) t)
            :to-equal 2)
    (expect (cl-regex-kit::ast-fixed-length captured t) :to-equal 1)
    (expect (cl-regex-kit::ast-fixed-length
             (make-instance 'cl-regex-kit::atomic-node :child two) t)
            :to-equal 2)
    (expect (cl-regex-kit::ast-fixed-length unknown t) :to-be-null)))

(it "annotates nested lookbehinds and counts capture shapes consistently"
  (labels ((literal (character)
             (make-instance 'cl-regex-kit::literal-node :char character))
           (capture (index)
             (make-instance 'cl-regex-kit::group-node
                            :child (literal #\x)
                            :capture-index index))
           (count-of (node)
             (multiple-value-list (cl-regex-kit::ast-static-capture-count node))))
    (let* ((backward (make-instance 'cl-regex-kit::lookaround-node
                                    :kind :lookbehind
                                    :child (make-instance 'cl-regex-kit::concat-node
                                                          :children (list (literal #\a)
                                                                           (literal #\b)))
                                    :direction :backward))
           (condition-backward
             (make-instance 'cl-regex-kit::assertion-node
                            :child (literal #\q)
                            :direction :backward))
           (forward (make-instance 'cl-regex-kit::assertion-node
                                   :child (literal #\z)
                                   :direction :forward))
           (conditional (make-instance 'cl-regex-kit::conditional-node
                                       :condition condition-backward
                                       :yes-branch backward
                                       :no-branch forward))
           (subroutine (make-instance 'cl-regex-kit::subroutine-node
                                      :target (make-instance 'cl-regex-kit::assertion-node
                                                             :child (literal #\r)
                                                             :direction :backward)))
           (root (make-instance 'cl-regex-kit::concat-node
                                :children (list conditional subroutine backward
                                                 backward))))
      (expect (cl-regex-kit::annotate-lookbehind-lengths root nil)
              :to-be root)
      (expect (cl-regex-kit::assertion-node-fixed-length backward)
              :to-equal 2)
      (expect (cl-regex-kit::assertion-node-fixed-length condition-backward)
              :to-equal 1)
      (expect (cl-regex-kit::assertion-node-fixed-length
               (cl-regex-kit::subroutine-node-target subroutine))
              :to-equal 1)
      (expect (cl-regex-kit::assertion-node-fixed-length forward)
              :to-be-null)
      (expect (cl-regex-kit::ast-group-count (capture 7)) :to-equal 7)
      (expect (cl-regex-kit::ast-group-count
               (make-instance 'cl-regex-kit::concat-node
                              :children (list (capture 2) (capture 9))))
              :to-equal 9)
      (expect (cl-regex-kit::ast-group-count
               (make-instance 'cl-regex-kit::alternation-node
                              :branches (list (capture 3) (capture 4))))
              :to-equal 4)
      (expect (cl-regex-kit::ast-group-count
               (make-instance 'cl-regex-kit::conditional-node
                              :yes-branch (capture 6)
                              :no-branch (capture 8)))
              :to-equal 8)
      (expect (cl-regex-kit::ast-group-count
               (make-instance 'cl-regex-kit::subroutine-node
                              :target (capture 10)))
              :to-equal 10)
      (expect (cl-regex-kit::ast-group-count
               (make-instance 'cl-regex-kit::regex-node)) :to-equal 0)
      (expect (count-of (make-instance 'cl-regex-kit::concat-node
                                       :children nil))
              :to-equal '(0 t))
      (expect (count-of (make-instance 'cl-regex-kit::alternation-node
                                       :branches nil))
              :to-equal '(0 t))
      (expect (count-of (make-instance 'cl-regex-kit::repetition-node
                                       :child (capture 1) :min 2 :max 2))
              :to-equal '(1 t))
      (expect (count-of (make-instance 'cl-regex-kit::repetition-node
                                       :child (capture 1) :min 1 :max nil))
              :to-equal '(1 t))
      (expect (count-of (make-instance 'cl-regex-kit::possessive-repetition-node
                                       :child (capture 1) :min 0 :max nil))
              :to-equal '(nil nil))
      (expect (count-of (make-instance 'cl-regex-kit::possessive-repetition-node
                                       :child (literal #\a) :min 0 :max nil))
              :to-equal '(0 t))
      (expect (count-of (make-instance 'cl-regex-kit::assertion-node
                                       :child (capture 1)))
              :to-equal '(1 t))
      (expect (count-of (make-instance 'cl-regex-kit::assertion-node
                                       :child (capture 1) :negative-p t))
              :to-equal '(0 t))
      (expect (count-of (make-instance 'cl-regex-kit::atomic-node
                                       :child (capture 1)))
              :to-equal '(1 t))
      (expect (count-of (make-instance 'cl-regex-kit::conditional-node
                                       :yes-branch (capture 1)
                                       :no-branch (capture 2)))
              :to-equal '(1 t))
      (expect (count-of (make-instance 'cl-regex-kit::conditional-node
                                       :yes-branch (capture 1)
                                       :no-branch (literal #\a)))
              :to-equal '(nil nil))
      (expect (count-of (make-instance 'cl-regex-kit::conditional-node
                                       :yes-branch (literal #\a)))
              :to-equal '(0 t))
      (expect (count-of (make-instance 'cl-regex-kit::regex-node))
              :to-equal '(0 t)))))
