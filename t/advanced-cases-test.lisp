(in-package #:cl-regex-kit/test)

(it-match-cases
 "matches ordered alternatives from one compiled advanced pattern"
 "(?:(?<word>cat)|dog)"
 :matches ("cat" "dog" "a dog in a sentence")
 :non-matches ("cow" "bird"))

(it-match-cases
 "keeps a backreference tied to the participating capture"
 "(?<word>[a-z]+)-\\k<word>"
 :matches ("echo-echo" "test-test")
 :non-matches ("echo-test" "test-echo"))

(it
 "loads the advanced runner as a separate ASDF component"
 (expect (fboundp 'run-advanced-regex) :to-be-truthy)
 (let ((regex (compile-regex "(?=a)a")))
   (expect (regex-advanced-p regex) :to-be-truthy)
   (expect "a" :to-match-regex regex)))

(it
 "evaluates any-character and line-break nodes on the advanced path"
 (let ((any-character (compile-regex "(?=.)a"))
       (line-break (compile-regex "(?=\\R)\\R")))
   (dolist (case (list (list any-character "a")
                       (list line-break (string (code-char 10)))))
     (destructuring-bind (regex text) case
       (let ((result (scan regex text)))
         (expect
          (and result
               (list (match-start result) (match-end result)))
          :to-equal
          '(0 1)))))))

(it
 "rejects invalid advanced runner limits"
 (let ((regex (compile-regex "(?=a)a")))
   (signals error
     (run-advanced-regex regex "a" :step-limit 0))
   (signals error
     (run-advanced-regex regex "a" :nest-limit -1))))

(it
 "compares advanced character and byte elements with their mode rules"
 (expect
  (cl-regex-kit::%advanced-element-equal-p #\A #\a t nil)
  :to-be-truthy)
 (expect
  (cl-regex-kit::%advanced-element-equal-p 65 97 t nil)
  :to-be-truthy)
 (expect
  (cl-regex-kit::%advanced-element-equal-p #\A #\a nil nil)
  :to-be-null)
 (expect
  (cl-regex-kit::%advanced-element-equal-p #\A 65 t nil)
  :to-be-null))

(it
 "matches Unicode grapheme clusters across UAX boundary rules"
 (let ((regex (compile-regex "\\X")))
   (dolist (case
             (list
              (list "CRLF" (format nil "~C~C" (code-char 13) (code-char 10)) 2)
              (list "regional indicators"
                    (coerce (list (code-char #x1f1e6)
                                  (code-char #x1f1e7)
                                  (code-char #x1f1e8))
                            'string)
                    2)
              (list "ZWJ emoji"
                    (coerce (list (code-char #x1f469)
                                  (code-char #x200d)
                                  (code-char #x1f4bb))
                            'string)
                    3)
              (list "Hangul jamo"
                    (coerce (list (code-char #x1100)
                                  (code-char #x1161)
                                  (code-char #x11a8))
                            'string)
                    3)
              (list "spacing mark"
                    (coerce (list (code-char #x0915)
                                  (code-char #x093e))
                            'string)
                    2)
              (list "prepend"
                    (coerce (list (code-char #x0600) #\A) 'string)
                    2)))
     (destructuring-bind (name text expected-end) case
       (declare (ignore name))
       (let ((result (scan regex text)))
         (expect
          (and result
               (list (match-start result) (match-end result)))
          :to-equal
          (list 0 expected-end)))))))

(it
 "recognizes sentence boundaries around terminal punctuation and separators"
 (let ((regex (compile-regex "\\b{sb}B")))
   (dolist (case '( ("A. B" (3 4))
                   ("A. )B" (4 5))
                   ("A. !B" (4 5))
                   ("A.   B" (5 6))
                   ("A. 1B" nil)
                   ("A. b" nil)))
     (destructuring-bind (text expected) case
       (let ((result (scan regex text)))
         (expect
          (and result
               (list (match-start result) (match-end result)))
          :to-equal
          expected))))))

(it
 "honors commit control across ordered alternatives"
 (let ((committed (compile-regex "a(*COMMIT)b|ac"))
       (commit-failure (compile-regex "a(*COMMIT)(*FAIL)|b"))
       (standalone-commit (compile-regex "(*COMMIT)")))
   (expect "ab" :to-match-regex committed)
   (expect (scan committed "ac") :to-be-null)
   (expect (scan commit-failure "ab") :to-be-null)
   (expect "b" :to-match-regex commit-failure)
   (expect "a" :to-match-regex standalone-commit)
   (signals error
     (run-advanced-regex
      (compile-regex "(?=a)a")
      "a"
      :shortest-p t
      :longest-p t))))

(it
 "reports the public step limit when advanced execution is bounded"
 (let ((condition
         (handler-case
             (progn
               (scan (compile-regex "(?=a)a" :size-limit 1) "a")
               nil)
           (advanced-regex-limit-error (condition)
             condition))))
   (expect condition :to-be-truthy)
   (when condition
     (expect (advanced-regex-limit-kind condition) :to-equal :steps)
     (expect (advanced-regex-limit condition) :to-equal 1)
     (expect (advanced-regex-limit-used condition) :to-equal 2))))

(it
 "reports the public nest-depth limit when advanced execution is bounded"
 (let ((condition
         (handler-case
             (progn
               (scan (compile-regex "(?=a)a" :nest-limit 1) "a")
               nil)
           (advanced-regex-limit-error (condition)
             condition))))
   (expect condition :to-be-truthy)
   (when condition
     (expect (advanced-regex-limit-kind condition) :to-equal :nest-depth)
     (expect (advanced-regex-limit condition) :to-equal 1)
     (expect (advanced-regex-limit-used condition) :to-equal 2))))

(it
 "applies explicit ranges and result selection in the advanced runner"
 (let ((regex (compile-regex "(?=a)a+")))
   (dolist (case '(((:start 1 :end 4 :shortest-p t) 1 2)
                   ((:start 1 :end 4 :longest-p t) 1 4)
                   ((:start 2 :end 3) 2 3)
                   ((:start 3 :end 3) nil nil)))
     (destructuring-bind (arguments expected-start expected-end) case
       (let ((result (apply (function run-advanced-regex)
                            regex
                            "baaa"
                            arguments)))
         (expect
          (and result
               (list (match-start result) (match-end result)))
          :to-equal
          (and expected-start
               (list expected-start expected-end))))))))

(it-match-cases
 "evaluates assertion conditions in the advanced matcher"
 "(?(?=a)a|b)"
 :matches ("a" "b")
 :non-matches ("c"))

(it
 "evaluates named capture conditions in both branches"
 (let ((regex (compile-regex "(?<a>a)?(?(<a>)b|c)")))
   (dolist (case '(("ab" "ab") ("c" "c")))
     (destructuring-bind (text expected) case
       (let ((result (scan regex text)))
         (expect (and result (match-string result text)) :to-equal expected))))
   (expect (scan regex "ad") :to-be-null)))

(it-match-cases
 "evaluates recursion conditions outside and inside subroutines"
 "(?(R)b|c)"
 :matches ("c")
 :non-matches ("b"))

(it-match-cases
 "evaluates recursive named conditions"
 "(?<p>a(?:(?&p))?(?(R)b|c))"
 :matches ("ac" "aabc")
 :non-matches ("aab"))

(it-match-cases
 "evaluates recursive index conditions"
 "(?<p>a(?:(?&p))?(?(R1)b|c))"
 :matches ("ac" "aabc")
 :non-matches ("aab"))

(it-match-cases
 "evaluates recursive name conditions"
 "(?<p>a(?:(?&p))?(?(R&p)b|c))"
 :matches ("ac" "aabc")
 :non-matches ("aab"))

(it-each
    (("(*POSITIVE_LOOKAHEAD:a)a" "a" "b")
     ("(*PLA:a)a" "a" "b")
     ("(*NEGATIVE_LOOKAHEAD:b)a" "a" "b")
     ("(*NLA:b)a" "a" "b")
     ("(*POSITIVE_LOOKBEHIND:a)b" "ab" "ba")
     ("(*PLB:a)b" "ab" "ba")
     ("(*NEGATIVE_LOOKBEHIND:b)a" "a" "ba")
     ("(*NLB:b)a" "a" "ba")
     ("(*NON_ATOMIC_POSITIVE_LOOKAHEAD:a)a" "a" "b")
     ("(*NAPLA:a)a" "a" "b")
     ("(*NON_ATOMIC_POSITIVE_LOOKBEHIND:a)b" "ab" "ba")
     ("(*NAPLB:a)b" "ab" "ba"))
  "matches named lookaround ~S and rejects ~S"
  (pattern matching non-matching)
  (let ((regex (compile-regex pattern)))
    (expect matching :to-match-regex regex)
    (expect-not non-matching :to-match-regex regex)))

(it-parses-cases
 "parses recursive, relative, and conditional advanced references"
 ("(?R)"
  "(?0)"
  "(a)(?P=name)"
  "(?<word>a)(?P>word)"
  "(?<a>a)(?-1)"
  "(a)(?1)"
  "(?<p>a(?R)?b)"
  "(?(?=a)b|c)"
  "(a)?(?(<a>)b|c)"
  "(?(R)b|c)"
  "(?(R1)b|c)"
  "(?<p>a)(?(R&p)b|c)"
  "(?|a)"))

(it "covers advanced state, capture, and byte-element contracts"
  (flet ((context (text &key root (group-names nil) (byte-mode-p nil)
                         (callout nil) (limit nil))
           (let ((actual-limit (or limit (length text))))
             (cl-regex-kit::make-advanced-context
              :text text
              :search-start 0
              :limit actual-limit
              :text-length (length text)
              :byte-mode-p byte-mode-p
              :never-newline-p nil
              :root root
              :group-count 4
              :group-names group-names
              :step-limit most-positive-fixnum
              :steps 0
              :state-limit most-positive-fixnum
              :state-count 0
              :nest-limit most-positive-fixnum
              :callout callout)))
         (state (position slots &key control skip-to mark
                                  (reported-start nil) (recursion-depth nil)
                                  (recursion-target nil) (committed-p nil))
           (cl-regex-kit::%make-advanced-state
            position slots control skip-to mark reported-start recursion-depth
            recursion-target committed-p))
         (group (name index)
           (make-instance 'cl-regex-kit::group-node
                          :name name
                          :capture-index index
                          :child (make-instance 'cl-regex-kit::literal-node
                                                :char #\a))))
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
                                               :initial-contents '(nil nil 2 3)))))
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
               "missing" ctx)
              :to-be nil)
      (let ((cl-regex-kit::*advanced-context* ctx)
            (source (state 1 slots :mark "mark")))
        (let ((copy (cl-regex-kit::%advanced-state-copy source)))
          (expect (cl-regex-kit::advanced-state-position copy) :to-equal 1)
          (expect (cl-regex-kit::advanced-context-state-count ctx) :to-equal 1))
        (setf (cl-regex-kit::advanced-context-state-limit ctx) 1)
        (signals advanced-regex-limit-error
          (cl-regex-kit::%advanced-state-copy source))))
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

(it "covers sentence, grapheme, and special boundary contracts"
  (flet ((context (text &key (byte-mode-p nil))
           (cl-regex-kit::make-advanced-context
            :text text
            :search-start 0
            :limit (length text)
            :text-length (length text)
            :byte-mode-p byte-mode-p
            :never-newline-p nil
            :root nil
            :group-count 0
            :group-names nil
            :step-limit most-positive-fixnum
            :steps 0
            :state-limit most-positive-fixnum
            :state-count 0
            :nest-limit most-positive-fixnum
            :callout nil))
         (sentence-units (&rest classes)
           (coerce
            (loop for class in classes
                  for index from 0
                  collect (cl-regex-kit::make-advanced-sentence-unit
                           :start index :end (1+ index) :class class))
            'vector))
         (grapheme-unit (class &key (indic "NONE") (pictographic-p nil))
           (cl-regex-kit::make-advanced-grapheme-unit
            :character #\a
            :start 0
            :end 1
            :class class
            :indic-conjunct-break indic
            :extended-pictographic-p pictographic-p)))
    (expect (cl-regex-kit::%advanced-sentence-boundary-rule
             (sentence-units :cr :lf) 0 1)
            :to-be nil)
    (expect (cl-regex-kit::%advanced-sentence-boundary-rule
             (sentence-units :cr :upper) 0 1)
            :to-be-truthy)
    (expect (cl-regex-kit::%advanced-sentence-boundary-rule
             (sentence-units :aterm :numeric) 0 1)
            :to-be nil)
    (expect (cl-regex-kit::%advanced-sentence-boundary-rule
             (sentence-units :upper :aterm :upper) 1 2)
            :to-be nil)
    (expect (cl-regex-kit::%advanced-sentence-boundary-rule
             (sentence-units :aterm :lower) 0 1)
            :to-be nil)
    (expect (cl-regex-kit::%advanced-sentence-boundary-rule
             (sentence-units :sterm :sp :scontinue) 1 2)
            :to-be nil)
    (expect (cl-regex-kit::%advanced-sentence-boundary-rule
             (sentence-units :sterm :close) 0 1)
            :to-be nil)
    (expect (cl-regex-kit::%advanced-sentence-boundary-rule
             (sentence-units :sterm :sp :sep) 1 2)
            :to-be nil)
    (expect (cl-regex-kit::%advanced-sentence-boundary-rule
             (sentence-units :sterm :sp :upper) 1 2)
            :to-be-truthy)
    (expect (cl-regex-kit::%advanced-sentence-boundary-rule
             (sentence-units :sterm :upper) 0 1)
            :to-be-truthy)
    (expect (cl-regex-kit::%advanced-sentence-boundary-rule
             (sentence-units :upper :lower) 0 1)
            :to-be nil)
    (expect (cl-regex-kit::%advanced-sentence-boundary-between-p
             (sentence-units :cr :lf) 1)
            :to-be nil)
    (expect (cl-regex-kit::%advanced-sentence-boundary-between-p
             (sentence-units :cr :upper) 1)
            :to-be-truthy)
    (expect (cl-regex-kit::%advanced-sentence-boundary-between-p
             (sentence-units :upper :extend :lower) 1)
            :to-be nil)
    (let ((calls 0)
          (ctx (context "A. B")))
      (expect (cl-regex-kit::%advanced-boundary-cache
               ctx :test (lambda () (incf calls)))
              :to-equal 1)
      (expect (cl-regex-kit::%advanced-boundary-cache
               ctx :test (lambda () (incf calls)))
              :to-equal 1)
      (expect calls :to-equal 1)
      (expect (cl-regex-kit::%advanced-sentence-boundary-p ctx 0 t)
              :to-be-truthy)
      (expect (cl-regex-kit::%advanced-sentence-boundary-p ctx -1 t)
              :to-be nil)
      (expect (cl-regex-kit::%advanced-sentence-boundary-p ctx 100 t)
              :to-be nil)
      (expect (cl-regex-kit::%advanced-sentence-boundary-p ctx 100 nil)
              :to-be-truthy))
    (let ((invalid
            (context (make-array 1
                                 :element-type '(unsigned-byte 8)
                                 :initial-contents '(#xFF))
                     :byte-mode-p t)))
      (expect (cl-regex-kit::%advanced-sentence-boundary-p invalid 0 t)
              :to-be-truthy)
      (expect (cl-regex-kit::%advanced-sentence-boundary-p invalid 1 t)
              :to-be-truthy))
    (expect (cl-regex-kit::%advanced-grapheme-break-p
             (list (grapheme-unit "CR")) (grapheme-unit "LF") t)
            :to-be nil)
    (expect (cl-regex-kit::%advanced-grapheme-break-p
             (list (grapheme-unit "LF")) (grapheme-unit "OTHER") t)
            :to-be-truthy)
    (expect (cl-regex-kit::%advanced-grapheme-break-p
             (list (grapheme-unit "L")) (grapheme-unit "V") t)
            :to-be nil)
    (expect (cl-regex-kit::%advanced-grapheme-break-p
             (list (grapheme-unit "LV")) (grapheme-unit "T") t)
            :to-be nil)
    (expect (cl-regex-kit::%advanced-grapheme-break-p
             (list (grapheme-unit "LVT")) (grapheme-unit "T") t)
            :to-be nil)
    (expect (cl-regex-kit::%advanced-grapheme-break-p
             (list (grapheme-unit "OTHER")) (grapheme-unit "EXTEND") t)
            :to-be nil)
    (expect (cl-regex-kit::%advanced-grapheme-break-p
             (list (grapheme-unit "OTHER")) (grapheme-unit "ZWJ") t)
            :to-be nil)
    (expect (cl-regex-kit::%advanced-grapheme-break-p
             (list (grapheme-unit "OTHER")) (grapheme-unit "SPACINGMARK") t)
            :to-be nil)
    (expect (cl-regex-kit::%advanced-grapheme-break-p
             (list (grapheme-unit "PREPEND")) (grapheme-unit "OTHER") t)
            :to-be nil)
    (expect (cl-regex-kit::%advanced-grapheme-break-p
             (list (grapheme-unit "LINKER" :indic "LINKER")
                   (grapheme-unit "EXTEND" :indic "EXTEND")
                   (grapheme-unit "OTHER" :indic "CONSONANT"))
             (grapheme-unit "OTHER" :indic "CONSONANT") t)
            :to-be nil)
    (expect (cl-regex-kit::%advanced-grapheme-break-p
             (list (grapheme-unit "ZWJ")
                   (grapheme-unit "EXTEND")
                   (grapheme-unit "OTHER" :pictographic-p t))
             (grapheme-unit "OTHER" :pictographic-p t) t)
            :to-be nil)
    (expect (cl-regex-kit::%advanced-grapheme-break-p
             (list (grapheme-unit "RI")) (grapheme-unit "RI") t)
            :to-be nil)
    (expect (cl-regex-kit::%advanced-grapheme-break-p
             (list (grapheme-unit "RI") (grapheme-unit "RI"))
             (grapheme-unit "RI") t)
            :to-be-truthy)
    (expect (cl-regex-kit::%advanced-grapheme-break-p
             (list (grapheme-unit "OTHER")) (grapheme-unit "EXTEND") nil)
            :to-be nil)
    (expect (cl-regex-kit::%advanced-grapheme-break-p
             (list (grapheme-unit "OTHER")) (grapheme-unit "OTHER") nil)
            :to-be-truthy)
    (expect (cl-regex-kit::%advanced-grapheme-class 65) :to-equal "OTHER")
    (expect (cl-regex-kit::%advanced-indic-conjunct-break-class
             #\a "EXTEND")
            :to-equal "EXTEND")
    (expect (cl-regex-kit::%advanced-indic-conjunct-break-class
             #\a "OTHER")
            :to-equal "NONE")
    (let ((string-context (context "a "))
          (invalid-byte-context
            (context (make-array 2
                                 :element-type '(unsigned-byte 8)
                                 :initial-contents '(#xFF 65))
                     :byte-mode-p t)))
      (expect (cl-regex-kit::%advanced-grapheme-boundary-p
               string-context -1 t)
              :to-be nil)
      (expect (cl-regex-kit::%advanced-grapheme-boundary-p
               string-context 0 t)
              :to-be-truthy)
      (expect (cl-regex-kit::%advanced-grapheme-boundary-p
               string-context 1 t)
              :to-be-truthy)
      (expect (cl-regex-kit::%advanced-grapheme-boundary-p
               string-context 2 t)
              :to-be-truthy)
      (expect (cl-regex-kit::%advanced-grapheme-boundary-p
               invalid-byte-context 1 t)
              :to-be-truthy)
      (expect (cl-regex-kit::%advanced-grapheme-boundary-p
               string-context 1 nil)
              :to-be-truthy)
      (expect (cl-regex-kit::%advanced-word-boundary-p
               string-context 1 t)
              :to-be-truthy)
      (expect (cl-regex-kit::%advanced-word-boundary-p
               (context "a ") 1 nil)
              :to-be-truthy)
      (expect (cl-regex-kit::%advanced-word-boundary-p
               (context (make-array 2
                                   :element-type '(unsigned-byte 8)
                                   :initial-contents '(65 32))
                        :byte-mode-p t)
               1 nil)
              :to-be-truthy)
      (expect (cl-regex-kit::%advanced-word-boundary-p
               (context (make-array 2
                                   :element-type '(unsigned-byte 8)
                                   :initial-contents '(65 32))
                        :byte-mode-p t)
               1 t)
              :to-be-truthy)
      (dolist (node
               (list
                (make-instance 'cl-regex-kit::anchor-node
                               :kind :grapheme-boundary :unicode-p t)
                (make-instance 'cl-regex-kit::anchor-node
                               :kind :word-boundary-unicode :unicode-p t)
                (make-instance 'cl-regex-kit::anchor-node
                               :kind :sentence-boundary :unicode-p t)))
        (expect (cl-regex-kit::%advanced-special-boundary-p
                 node string-context 0)
                :to-be-truthy))
      (expect (cl-regex-kit::%advanced-special-boundary-p
               (make-instance 'cl-regex-kit::anchor-node :kind :unknown)
               string-context 0)
              :to-be nil))))

(it "covers advanced anchors, controls, callouts, and conditions"
  (flet ((context (text &key root (group-names nil) (callout nil))
           (cl-regex-kit::make-advanced-context
            :text text
            :search-start 0
            :limit (length text)
            :text-length (length text)
            :byte-mode-p nil
            :never-newline-p nil
            :root root
            :group-count 4
            :group-names group-names
            :step-limit most-positive-fixnum
            :steps 0
            :state-limit most-positive-fixnum
            :state-count 0
            :nest-limit most-positive-fixnum
            :callout callout))
         (state (position slots &key control skip-to mark
                                  (recursion-depth nil) (recursion-target nil)
                                  (committed-p nil))
           (cl-regex-kit::%make-advanced-state
            position slots control skip-to mark nil recursion-depth
            recursion-target committed-p)))
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

(it "covers advanced evaluator control and boundary branches"
  (flet ((context (text &key root (group-names nil) (byte-mode-p nil)
                         (callout nil) (limit nil))
           (let ((actual-limit (or limit (length text))))
             (cl-regex-kit::make-advanced-context
              :text text
              :search-start 0
              :limit actual-limit
              :text-length (length text)
              :byte-mode-p byte-mode-p
              :never-newline-p nil
              :root root
              :group-count 4
              :group-names group-names
              :step-limit most-positive-fixnum
              :steps 0
              :state-limit most-positive-fixnum
              :state-count 0
              :nest-limit most-positive-fixnum
              :callout callout)))
         (state (position slots &key control skip-to mark
                                  (reported-start nil)
                                  (recursion-depth nil)
                                  (recursion-target nil)
                                  (committed-p nil))
           (cl-regex-kit::%make-advanced-state
            position
            slots
            control
            skip-to
            mark
            reported-start
            recursion-depth
            recursion-target
            committed-p))
         (sentence-units (&rest classes)
           (coerce
            (mapcar
             (lambda (class)
               (cl-regex-kit::make-advanced-sentence-unit
                :start 0 :end 1 :class class))
             classes)
            'vector)))
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
               (let ((cl-regex-kit::*advanced-context* ctx))
                 (cl-regex-kit::%advanced-evaluate-concat
                  nil
                  (state 0 nil :control :commit)
                  ctx
                  0)))))
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
             (list (list (code-char #x093f) "EXTEND" "EXTEND")
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
        (run-advanced-regex regex "a"))
      (setf (slot-value regex 'cl-regex-kit::advanced-step-limit) 1
            (slot-value regex 'cl-regex-kit::advanced-nest-limit) -1)
      (signals error
        (run-advanced-regex regex "a")))))
)
