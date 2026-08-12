(in-package #:cl-regex-kit/test)

(it-advanced-pattern-cases
 "tokenized non-atomic lookarounds compile as advanced expressions"
 ("(?*a|ab)c"
  "(?<*a|b)c"
  "(*PLA:a)b"
  "(*NLA:a)b"
  "(*PLB:a)b"
  "(*NLB:a)b"
  "(*NAPLA:a|ab)c"
  "(*NAPLB:a|ab)c"))

(it "returns a boolean advanced-engine flag"
  (let ((ordinary (regex-advanced-p (compile-regex "a")))
        (lookahead (regex-advanced-p (compile-regex "(?=a)a")))
        (balanced (regex-advanced-p (compile-regex "(?<-name>b)")))
        (anchor (regex-advanced-p (compile-regex "\\G"))))
    (expect-equal-cases
      (ordinary nil)
      (lookahead t)
      (balanced t)
      (anchor t))
    (expect-truthy-cases
      (typep ordinary 'boolean)
      (typep lookahead 'boolean)
      (typep balanced 'boolean)
      (typep anchor 'boolean))))

(it-pattern-match-string-cases
 "positive non-atomic assertions retain a backtrackable capture"
 (("(?*(a|ab))\\1c" "abc" "abc")
  ("(*NAPLA:(a|ab))\\1c" "abc" "abc")
  ("(?<*(a|ab))\\1" "abab" "ab")
  ("(*NAPLB:(a|ab))\\1" "abab" "ab")))

(it "keeps atomic groups distinct from non-atomic assertions"
  (expect (match "(?>a|ab)\\1c" "abc") :to-be-null))

(it "deep-copies advanced capture stacks and preserves state metadata"
  (let* ((stack (list (cons 2 3) (cons 0 1)))
         (stacks (vector stack nil))
         (slots (vector 10 11 stacks))
         (copy (cl-regex-kit::%advanced-copy-slots slots))
         (state (cl-regex-kit::%make-advanced-state
                 4 slots :commit 7 "mark" 2 3 5 t))
         (state-copy (cl-regex-kit::%advanced-state-copy state)))
    (setf (car (car (aref (aref copy 2) 0))) 99)
    (setf (aref (cl-regex-kit::advanced-state-slots state-copy) 0) 77)
    (expect (car (car (aref (aref slots 2) 0))) :to-equal 2)
    (expect (car (car (aref (aref copy 2) 0))) :to-equal 99)
    (expect (cl-regex-kit::advanced-state-position state-copy) :to-equal 4)
    (expect (cl-regex-kit::advanced-state-control state-copy) :to-equal :commit)
    (expect (cl-regex-kit::advanced-state-skip-to state-copy) :to-equal 7)
    (expect (cl-regex-kit::advanced-state-mark state-copy) :to-equal "mark")
    (expect (aref (cl-regex-kit::advanced-state-slots state-copy) 0) :to-equal 77)
    (expect (aref (cl-regex-kit::advanced-state-slots state) 0) :to-equal 10)))

(it "initializes and synchronizes the top capture stack"
  (let ((slots (make-array 5 :initial-element nil)))
    (cl-regex-kit::%advanced-initialize-capture-stacks slots 1)
    (let ((stacks (aref slots 4)))
      (expect (vectorp stacks) :to-be-truthy)
      (expect (length stacks) :to-equal 2)
      (setf (aref stacks 1) (list (cons 8 9)))
      (let ((state (cl-regex-kit::%make-advanced-state 0 slots)))
        (cl-regex-kit::%advanced-sync-capture-slot state 1)
        (expect (aref slots 2) :to-equal 8)
        (expect (aref slots 3) :to-equal 9)
        (expect (cl-regex-kit::%advanced-capture-stacks state) :to-be stacks)))))

(it "classifies terminal advanced controls without treating normal states as terminal"
  (dolist (control '(:accept :commit :commit-failure :prune :skip :then))
    (let ((state (cl-regex-kit::%make-advanced-state 0 #() control)))
      (expect-truthy-cases
        (cl-regex-kit::%advanced-state-terminal-p state))
      (expect-falsy-cases
        (cl-regex-kit::%advanced-state-normal-p state))))
  (let ((state (cl-regex-kit::%make-advanced-state 0 #())))
    (expect-falsy-cases
      (cl-regex-kit::%advanced-state-terminal-p state))
    (expect-truthy-cases
      (cl-regex-kit::%advanced-state-normal-p state))))

(it "reports step and nesting resource limits with their measured usage"
  (let ((step-context (cl-regex-kit::make-advanced-context
                       :steps 0 :step-limit 1 :nest-limit 4))
        (nest-context (cl-regex-kit::make-advanced-context
                       :steps 0 :step-limit 4 :nest-limit 2))
        step-condition
        nest-condition)
    (handler-case
        (progn
          (cl-regex-kit::%advanced-step step-context 0)
          (cl-regex-kit::%advanced-step step-context 0))
      (advanced-regex-limit-error (condition)
        (setf step-condition condition)))
    (handler-case
        (cl-regex-kit::%advanced-step nest-context 3)
      (advanced-regex-limit-error (condition)
        (setf nest-condition condition)))
    (expect (advanced-regex-limit-kind step-condition) :to-equal :steps)
    (expect (advanced-regex-limit step-condition) :to-equal 1)
    (expect (advanced-regex-limit-used step-condition) :to-equal 2)
    (expect (princ-to-string step-condition)
            :to-equal
            "Advanced regex STEPS limit 1 exceeded after 2 units.")
    (expect (advanced-regex-limit-kind nest-condition) :to-equal :nest-depth)
    (expect (advanced-regex-limit nest-condition) :to-equal 2)
    (expect (advanced-regex-limit-used nest-condition) :to-equal 3)
    (expect (cl-regex-kit::advanced-context-steps nest-context) :to-equal 1)))

(it "reads Unicode strings, UTF-8 octets, and raw bytes through one input boundary"
  (let ((string-context (cl-regex-kit::make-advanced-context
                         :text (string (code-char #xe9))
                         :limit 1))
        (octets (make-array 2
                            :element-type '(unsigned-byte 8)
                            :initial-contents '(#xc3 #xa9))))
    (multiple-value-bind (element end valid-p)
        (cl-regex-kit::%advanced-read-element string-context 0 t)
      (expect element :to-equal (code-char #xe9))
      (expect end :to-equal 1)
      (expect valid-p :to-be-truthy))
    (multiple-value-bind (element end valid-p)
        (cl-regex-kit::%advanced-read-element string-context 1 t)
      (expect element :to-be-null)
      (expect end :to-be-null)
      (expect valid-p :to-be-falsy))
    (let ((octet-context (cl-regex-kit::make-advanced-context
                          :text octets :limit 2 :byte-mode-p t)))
      (multiple-value-bind (element end valid-p)
          (cl-regex-kit::%advanced-read-element octet-context 0 t)
        (expect element :to-equal (code-char #xe9))
        (expect end :to-equal 2)
        (expect valid-p :to-be-truthy))
      (multiple-value-bind (element end valid-p)
          (cl-regex-kit::%advanced-read-element octet-context 0 nil)
        (expect element :to-equal #xc3)
        (expect end :to-equal 1)
        (expect valid-p :to-be-truthy)))))

(it "uses Unicode-aware and byte-aware element equality deliberately"
  (expect (cl-regex-kit::%advanced-element-equal-p #\a #\A t t)
          :to-be-truthy)
  (expect (cl-regex-kit::%advanced-element-equal-p #\a #\A t nil)
          :to-be-truthy)
  (expect (cl-regex-kit::%advanced-element-equal-p #\a #\A nil t)
          :to-be-falsy)
  (expect (cl-regex-kit::%advanced-element-equal-p #x41 #x61 t nil)
          :to-be-truthy)
  (expect (cl-regex-kit::%advanced-element-equal-p #x41 #x61 nil nil)
          :to-be-falsy)
  (expect (cl-regex-kit::%advanced-element-equal-p #\a 97 t t)
          :to-be-falsy))

(it "finds nested groups across every advanced AST container"
  (let* ((target (make-instance 'cl-regex-kit::group-node
                                :child (make-instance 'cl-regex-kit::literal-node
                                                       :char #\x)
                                :capture-index 3
                                :name "Target"))
         (other (make-instance 'cl-regex-kit::group-node
                               :child (make-instance 'cl-regex-kit::literal-node
                                                      :char #\y)
                               :capture-index 1
                               :name "Other"))
         (wrapped-target
           (make-instance 'cl-regex-kit::repetition-node
                          :child
                          (make-instance 'cl-regex-kit::possessive-repetition-node
                                         :child
                                         (make-instance 'cl-regex-kit::assertion-node
                                                        :child
                                                        (make-instance 'cl-regex-kit::atomic-node
                                                                       :child target))
                                         :min 0
                                         :max 1)
                          :min 0
                          :max 1))
         (root (make-instance 'cl-regex-kit::concat-node
                              :children
                              (list
                               (make-instance 'cl-regex-kit::alternation-node
                                              :branches
                                              (list
                                               (make-instance 'cl-regex-kit::literal-node
                                                              :char #\z)
                                               wrapped-target))
                               (make-instance 'cl-regex-kit::conditional-node
                                              :condition :capture
                                              :yes-branch
                                              (make-instance 'cl-regex-kit::literal-node
                                                             :char #\q)
                                              :no-branch other))))
         (context (cl-regex-kit::make-advanced-context
                   :root root
                   :group-names '(("Target" . 3) ("target" . 4))))
         (slots (make-array 10 :initial-element nil))
         (state (cl-regex-kit::%make-advanced-state 0 slots)))
    (setf (aref slots 8) 12
          (aref slots 9) 15)
    (expect (cl-regex-kit::%advanced-find-group root :name "target")
            :to-be target)
    (expect (cl-regex-kit::%advanced-find-group root :index 3)
            :to-be target)
    (expect (cl-regex-kit::%advanced-find-group root :name "Other")
            :to-be other)
    (expect (cl-regex-kit::%advanced-find-group root :index 99)
            :to-be-null)
    (expect (cl-regex-kit::%advanced-group-matches-p target "target" nil)
            :to-be-truthy)
    (expect (cl-regex-kit::%advanced-group-matches-p target nil 3)
            :to-be-truthy)
    (expect (cl-regex-kit::%advanced-group-matches-p target nil 4)
            :to-be-falsy)
    (expect (cl-regex-kit::%advanced-capture-index-for-group target)
            :to-equal 3)
    (expect (cl-regex-kit::%advanced-capture-indices-by-name "TARGET" context)
            :to-equal '(3 4))
    (expect (cl-regex-kit::%advanced-capture-index-by-name "target" context state)
            :to-equal 4)
    (expect (cl-regex-kit::%advanced-capture-index-by-name "missing" context)
            :to-be-null)
    (let ((numeric (make-instance 'cl-regex-kit::backreference-node
                                  :capture-index 3))
          (named (make-instance 'cl-regex-kit::backreference-node
                                :name "target"))
          (fallback (make-instance 'cl-regex-kit::backreference-node
                                   :name "Other"))
          (fallback-context (cl-regex-kit::make-advanced-context :root root)))
      (expect (cl-regex-kit::%advanced-capture-index numeric context)
              :to-equal 3)
      (expect (cl-regex-kit::%advanced-capture-index named context state)
              :to-equal 4)
      (expect (cl-regex-kit::%advanced-capture-index fallback fallback-context)
              :to-equal 1))))

(it "builds public match results from advanced state slots"
  (let* ((slots (make-array 4 :initial-element nil))
         (state (cl-regex-kit::%make-advanced-state
                 5 slots nil nil (cons "tag" 0) nil nil nil nil)))
    (setf (aref slots 2) 3
          (aref slots 3) 4)
    (let ((result (cl-regex-kit::%advanced-result-from-state
                   state 1 1 '(("capture" . 1)))))
      (expect (match-start result) :to-equal 1)
      (expect (match-end result) :to-equal 5)
      (expect (match-mark result) :to-equal "tag")
      (expect (match-group-start result "capture") :to-equal 3)
      (expect (match-group-end result "capture") :to-equal 4)
      (expect (match-group-string result "capture" "abcdef") :to-equal "d"))))

(it "selects the first, shortest, or longest advanced state explicitly"
  (let ((states (list (cl-regex-kit::%make-advanced-state 4 #())
                      (cl-regex-kit::%make-advanced-state 2 #())
                      (cl-regex-kit::%make-advanced-state 7 #()))))
    (expect (cl-regex-kit::%advanced-result-end
             (cl-regex-kit::%advanced-select-result states nil nil))
            :to-equal 4)
    (expect (cl-regex-kit::%advanced-result-end
             (cl-regex-kit::%advanced-select-result states t nil))
            :to-equal 2)
    (expect (cl-regex-kit::%advanced-result-end
             (cl-regex-kit::%advanced-select-result states nil t))
            :to-equal 7)
    (expect (cl-regex-kit::%advanced-select-result nil t nil) :to-be-null)))

(it "supports short control-verb aliases through the public matcher"
  (expect (match "a(*F)|b" "b") :to-be-truthy)
  (expect (match "a(*F)" "a") :to-be-null)
  (expect (match "a(*A)b" "ab") :to-be-truthy)
  (expect (match "a(*C)b|ac" "ab") :to-be-truthy)
  (expect (match "a(*COMMIT)c|ab" "ab") :to-be-null)
  (expect (match "a(*COMMIT)c|ab" "ac") :to-be-truthy)
  (expect (match "a(*P)b|ac" "ac") :to-be-null)
  (expect (match "ab(*S)(*F)|b" "ab") :to-be-null))

(it "represents advanced control decisions as explicit state transitions"
  (flet ((node (verb &optional argument)
           (make-instance 'cl-regex-kit::control-verb-node
                          :verb verb
                          :argument argument))
         (state (&optional mark)
           (cl-regex-kit::%make-advanced-state 7 #() nil nil mark)))
    (expect (cl-regex-kit::%advanced-control-result
             (node "FAIL")
             (state))
            :to-be-null)
    (signals error
      (cl-regex-kit::%advanced-control-result
       (node "UNKNOWN")
       (state)))
    (let ((marked-state (state (cons "old" (list (cons "target" 4)))))
          (bare-state (state)))
      (let ((skip-to-mark
              (first
               (cl-regex-kit::%advanced-control-result
                (node "SKIP" "target")
                marked-state)))
            (skip-to-missing
              (first
               (cl-regex-kit::%advanced-control-result
                (node "SKIP" "missing")
                marked-state)))
            (skip-to-position
              (first
               (cl-regex-kit::%advanced-control-result
                (node "SKIP")
                bare-state)))
            (skip-to-integer
              (first
               (cl-regex-kit::%advanced-control-result
                (node "SKIP" 11)
                bare-state)))
            (prune-with-integer
              (first
               (cl-regex-kit::%advanced-control-result
                (node "PRUNE" 13)
                bare-state)))
            (prune-with-label
              (first
               (cl-regex-kit::%advanced-control-result
                (node "PRUNE" "label")
                bare-state))))
        (expect (cl-regex-kit::advanced-state-control skip-to-mark)
                :to-equal :skip)
        (expect (cl-regex-kit::advanced-state-skip-to skip-to-mark)
                :to-equal 4)
        (expect (cl-regex-kit::advanced-state-skip-to skip-to-missing)
                :to-be-null)
        (expect (cl-regex-kit::advanced-state-skip-to skip-to-position)
                :to-equal 7)
        (expect (cl-regex-kit::advanced-state-skip-to skip-to-integer)
                :to-equal 11)
        (expect (cl-regex-kit::advanced-state-control prune-with-integer)
                :to-equal :prune)
        (expect (cl-regex-kit::advanced-state-skip-to prune-with-integer)
                :to-equal 13)
        (expect (cl-regex-kit::advanced-state-skip-to prune-with-label)
                :to-be-null)))))

(it "evaluates recursion, capture, and embedded-node conditions explicitly"
  (let* ((target
           (make-instance 'cl-regex-kit::group-node
                          :child (make-instance 'cl-regex-kit::literal-node
                                                 :char #\x)
                          :capture-index 3
                          :name "Target"))
         (context
           (cl-regex-kit::make-advanced-context
            :text "x"
            :limit 1
            :text-length 1
            :step-limit 100
            :steps 0
            :nest-limit 10
            :root target
            :group-names '(("Target" . 3))))
         (fallback-context
           (cl-regex-kit::make-advanced-context
            :text "x"
            :limit 1
            :text-length 1
            :step-limit 100
            :steps 0
            :nest-limit 10
            :root target))
         (slots (make-array 8 :initial-element nil))
         (state (cl-regex-kit::%make-advanced-state
                 0 slots nil nil nil nil 1 target))
         (normal-state (cl-regex-kit::%make-advanced-state 0 slots)))
    (setf (aref slots 6) 2
          (aref slots 7) 3)
    (expect (cl-regex-kit::%advanced-condition-true-p
             :define state context 0)
            :to-be-null)
    (expect (cl-regex-kit::%advanced-condition-true-p
             :recursion state context 0)
            :to-be-truthy)
    (expect (cl-regex-kit::%advanced-condition-true-p
             :recursion normal-state context 0)
            :to-be-null)
    (expect (cl-regex-kit::%advanced-condition-true-p
             '(:recursion-index 3) state context 0)
            :to-be-truthy)
    (expect (cl-regex-kit::%advanced-condition-true-p
             '(:recursion-index 4) state context 0)
            :to-be-null)
    (expect (cl-regex-kit::%advanced-condition-true-p
             '(:recursion-name "Target") state context 0)
            :to-be-truthy)
    (expect (cl-regex-kit::%advanced-condition-true-p
             '(:recursion-name "Other") state context 0)
            :to-be-null)
    (expect (cl-regex-kit::%advanced-condition-true-p
             '(:capture-index 3) state context 0)
            :to-be-truthy)
    (expect (cl-regex-kit::%advanced-condition-true-p
             '(:capture-index 4) state context 0)
            :to-be-null)
    (expect (cl-regex-kit::%advanced-condition-true-p
             '(:name "Target") state context 0)
            :to-be-truthy)
    (expect (cl-regex-kit::%advanced-condition-true-p
             '(:name "Target") state fallback-context 0)
            :to-be-truthy)
    (expect (cl-regex-kit::%advanced-condition-true-p
             '(:name "Missing") state fallback-context 0)
            :to-be-null)
    (expect (cl-regex-kit::%advanced-condition-true-p
             (make-instance 'cl-regex-kit::literal-node :char #\x)
             state context 0)
            :to-be-truthy)
    (expect (cl-regex-kit::%advanced-condition-true-p
             (make-instance 'cl-regex-kit::literal-node :char #\y)
             state context 0)
            :to-be-null)
    (expect (cl-regex-kit::%advanced-condition-true-p
             :unknown state context 0)
            :to-be-null)))

(it "resolves direct, numeric, named, and recursive subroutine targets"
  (let* ((target
           (make-instance 'cl-regex-kit::group-node
                          :child (make-instance 'cl-regex-kit::literal-node
                                                 :char #\x)
                          :capture-index 3
                          :name "Target"))
         (root (make-instance 'cl-regex-kit::concat-node
                              :children (list target)))
         (context (cl-regex-kit::make-advanced-context
                   :root root
                   :group-names '(("Target" . 3))))
         (direct (make-instance 'cl-regex-kit::subroutine-node
                                :target target))
         (zero (make-instance 'cl-regex-kit::subroutine-node :target 0))
         (numeric (make-instance 'cl-regex-kit::subroutine-node :target 3))
         (named (make-instance 'cl-regex-kit::subroutine-node
                               :target "Target"))
         (symbolic (make-instance 'cl-regex-kit::subroutine-node
                                  :target 'target))
         (recursive (make-instance 'cl-regex-kit::recursion-node))
         (missing (make-instance 'cl-regex-kit::subroutine-node :target 99))
         (unresolved (make-instance 'cl-regex-kit::subroutine-node)))
    (expect (cl-regex-kit::%advanced-subroutine-target direct context)
            :to-be target)
    (expect (cl-regex-kit::%advanced-subroutine-target zero context)
            :to-be root)
    (expect (cl-regex-kit::%advanced-subroutine-target numeric context)
            :to-be target)
    (expect (cl-regex-kit::%advanced-subroutine-target named context)
            :to-be target)
    (expect (cl-regex-kit::%advanced-subroutine-target symbolic context)
            :to-be target)
    (expect (cl-regex-kit::%advanced-subroutine-target recursive context)
            :to-be root)
    (expect (cl-regex-kit::%advanced-subroutine-target missing context)
            :to-be-null)
    (expect (cl-regex-kit::%advanced-subroutine-target unresolved context)
            :to-be-null)))

(it "evaluates generic consuming nodes and rejects unknown AST nodes"
  (let ((context (cl-regex-kit::make-advanced-context
                  :text (format nil "a~%")
                  :limit 2
                  :text-length 2
                  :step-limit 100
                  :steps 0
                  :nest-limit 10)))
    (expect
     (cl-regex-kit::advanced-state-position
      (first
       (cl-regex-kit::%advanced-node-evaluate
        (make-instance 'cl-regex-kit::any-char-node)
        (cl-regex-kit::%make-advanced-state 0 #())
        context
        0)))
     :to-equal 1)
    (expect
     (cl-regex-kit::advanced-state-position
      (first
       (cl-regex-kit::%advanced-node-evaluate
        (make-instance 'cl-regex-kit::line-break-node)
        (cl-regex-kit::%make-advanced-state 1 #())
        context
        0)))
     :to-equal 2)
    (signals error
      (cl-regex-kit::%advanced-node-evaluate
       (make-instance 'cl-regex-kit::regex-node)
       (cl-regex-kit::%make-advanced-state 0 #())
       context
       0))))

(it "rejects malformed advanced execution limits before matching"
  (flet ((regex-with-limits (step-limit nest-limit)
           (make-instance 'cl-regex-kit::regex
                          :program nil
                          :ast (make-instance 'cl-regex-kit::literal-node
                                              :char #\a)
                          :advanced-p t
                          :advanced-step-limit step-limit
                          :advanced-nest-limit nest-limit
                          :group-count 0
                          :static-capture-count 1
                          :group-names nil
                          :source "a")))
    (signals error
      (cl-regex-kit::run-advanced-regex
       (regex-with-limits 0 10)
       "a"))
    (signals error
      (cl-regex-kit::run-advanced-regex
       (regex-with-limits 10 -1)
       "a"))))

(it "handles explicit callout continuation and rejects invalid decisions"
  (let ((without-callback (compile-regex "(?C)a"))
        (continue
          (compile-regex "(?C)a"
                         :callout (lambda (&rest arguments)
                                    (declare (ignore arguments))
                                    nil)))
        (invalid
          (compile-regex "(?C)a"
                         :callout (lambda (&rest arguments)
                                    (declare (ignore arguments))
                                    :retry))))
    (expect (scan without-callback "a") :to-be-truthy)
    (expect (scan continue "a") :to-be-truthy)
    (signals error (scan invalid "a"))))

(it "rejects contradictory advanced result-selection options"
  (let ((regex (compile-regex "(?=a)a")))
    (signals error
      (cl-regex-kit::run-advanced-regex regex "a"
                                        :shortest-p t
                                        :longest-p t))))

(it "terminates non-progressing recursion before exhausting the nest limit"
  (let* ((recursive-fallback (compile-regex "(?:a|(?R))c"))
         (nested (compile-regex "(?<item>a(?1)?)"))
         (matching (scan recursive-fallback "ac"))
         (nested-result (scan nested "aaa")))
    (expect (match-string matching "ac") :to-equal "ac")
    (expect (scan recursive-fallback "bc") :to-be-null)
    (expect (match-string nested-result "aaa") :to-equal "aaa")))
