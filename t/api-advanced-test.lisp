;;;; t/api-advanced-test.lisp
(in-package #:cl-regex-kit/test)

(it-advanced-pattern-scan-cases
  "executes advanced ordered-backtracking patterns through public scan"
  (("(?=a)a" "a" 0 1)
   ("(?<=a)b" "ab" 1 2)
   ("a++" "aa" 0 2)
   ("a{2,2}+b" "aab" 0 3)
   ("(?>a|ab)c" "ac" 0 2)
   ("(?<word>a)\\g'word'" "aa" 0 2)))

(it
  "rejects ordered-backtracking paths that should fail through public scan"
  (expect (scan (compile-regex "a++a") "aa") :to-be-null)
  (expect (scan (compile-regex "(?>a|ab)c") "abc") :to-be-null))

(it
  "keeps negative lookarounds honest on both successful and failing paths"
  (let* ((negative-lookahead-success (scan (compile-regex "(?!a)b") "b"))
         (negative-lookahead-failure (scan (compile-regex "(?!a)a") "a"))
         (negative-lookbehind-success (scan (compile-regex "(?<!a)b") "b"))
         (negative-lookbehind-failure (scan (compile-regex "(?<!a)b") "ab")))
    (expect-match-span-cases
      (negative-lookahead-success 0 1)
      (negative-lookbehind-success 0 1))
    (expect negative-lookahead-failure :to-be-null)
    (expect negative-lookbehind-failure :to-be-null)))

(it
  "annotates only proven fixed-width lookbehinds"
  (labels ((width (pattern &key byte-mode)
             (let ((ast
                     (if byte-mode
                         (cl-regex-kit::regex-ast
                          (compile-byte-regex pattern))
                         (cl-regex-kit::regex-ast
                          (compile-regex pattern)))))
               (cl-regex-kit::assertion-node-fixed-length ast))))
    (expect (width "(?<=ab)") :to-equal 2)
    (expect (width "(?<=a|bc)") :to-be nil)
    (expect (width "(?<=a{2})") :to-equal 2)
    (expect (width "(?<=a+)") :to-be nil)
    (expect (width "(?<=é)" :byte-mode t) :to-equal 2)
    (expect (width "(?<=[é])" :byte-mode t) :to-be nil)))

(it-pattern-scan-cases
  "executes variable-length lookbehinds on successful paths"
  (("(?<=a+)b" "aaab" 3 4)
   ("(?<=a|bc)d" "bcd" 2 3)
   ("(?<!a+)b" "b" 0 1)))

(it
  "preserves captures and rejects failing variable-length lookbehinds"
  (expect (scan (compile-regex "(?<=a+)b") "b") :to-be-null)
  (expect (scan (compile-regex "(?<!a+)b") "aaab") :to-be-null)
  (let ((captured (scan (compile-regex "(?<=(a+))b") "aaab")))
    (expect (and captured
                 (list (match-group-start captured 1)
                       (match-group-end captured 1)
                       (match-group-string captured 1 "aaab")))
            :to-equal
            (list 0 3 "aaa"))))

(it
  "exposes MARK control-verb tags through the public match result"
  (let* ((regex (compile-regex "a(*MARK:middle)b"))
         (result (scan regex "ab")))
    (expect-advanced-regex-p-cases regex)
    (expect-match-string-cases
      (result "ab" "ab"))
    (expect (match-mark result) :to-equal "middle")))

(it
  "selects shortest and longest advanced matches"
  (let* ((selection (compile-regex "(?=a)(?:a|aa)"))
         (longest (longest-match selection "aa")))
    (expect-advanced-regex-p-cases selection)
    (expect (shortest-match selection "aa") :to-equal 1)
    (expect (and longest
                 (list (match-start longest)
                       (match-end longest)
                       (match-string longest "aa")))
            :to-equal
            (list 0 2 "aa"))))

(it
  "applies FAIL, ACCEPT, PRUNE, and THEN control verbs"
  (let ((fail (compile-regex "a(*FAIL)|b"))
        (accept (compile-regex "a(*ACCEPT)b"))
        (prune (compile-regex "a(*PRUNE)b|ac"))
        (then (compile-regex "a(*THEN)b|ac")))
    (expect-advanced-regex-p-cases fail accept prune then)
    (expect-match-string-cases
      ((scan fail "b") "b" "b")
      ((scan accept "ab") "ab" "a")
      ((scan then "ac") "ac" "ac"))
    (expect (scan prune "ac") :to-be-null)))

(it
  "applies bare and named SKIP control verbs"
  (let ((bare-skip (compile-regex "ab(*SKIP)(*FAIL)|b"))
        (named-skip
          (compile-regex
           "ab(*MARK:target)c(*SKIP:target)(*FAIL)|b|c"))
        (multiple-names
          (compile-regex
           "ab(*MARK:A)c(*MARK:B)d(*SKIP:A)(*FAIL)|b|c|d"))
        (missing-named-skip
          (compile-regex "ab(*SKIP:missing)(*FAIL)|b")))
    (expect-advanced-regex-p-cases
      bare-skip
      named-skip
      multiple-names
      missing-named-skip)
    (expect (scan bare-skip "ab") :to-be-null)
    (let ((result (scan named-skip "abc")))
      (expect (and result
                   (list (match-start result)
                         (match-end result)
                         (match-string result "abc")))
              :to-equal
              (list 2 3 "c")))
    (expect-match-string-cases
      ((scan multiple-names "abcd") "abcd" "c")
      ((scan missing-named-skip "ab") "ab" "b"))))

(it-advanced-public-scan-cases
  "covers cut and start anchors through public scan"
  '(("a\\Kb" "ab" 1 2 "b")
    ("\\Gabc" "zabc" 1 4 "abc" (:start 1))))

(it-advanced-public-scan-cases
  "covers grapheme clusters through public scan"
  (let ((grapheme (compile-regex "\\X"))
        (grapheme-text (format nil "a~C" (code-char #x301)))
        (indic-conjunct-text
          (format nil "~C~C~C"
                  (code-char #x915)
                  (code-char #x94D)
                  (code-char #x915))))
    (expect-advanced-regex-p-cases grapheme)
    (list (list "\\X" grapheme-text 0 2 grapheme-text)
          (list "\\X"
                indic-conjunct-text
                0
                3
                indic-conjunct-text))))

(it-advanced-public-scan-cases
  "covers branch-reset, conditionals, definitions, and recursion"
  '(("(?|(a)|(b))\\1" "bb" 0 2 "bb")
    ("(a)?(?(1)b|c)" "c" 0 1 "c")
    ("(a)?(?(1)b|c)" "ab" 0 2 "ab")
    ("(?(DEFINE)(?<word>[a-z]+))(?&word)" "abc" 0 3 "abc")
    ("(?<paren>\\((?:[^()]|(?&paren))*\\))" "(a(b))" 0 6 "(a(b))")))

(it
  "validates direct recursive and numeric subroutine targets"
  (let ((recursive (compile-regex "(?R)"))
        (zero (compile-regex "(?0)"))
        (numeric (compile-regex "(?<item>a)(?1)")))
    (expect-advanced-regex-p-cases recursive zero numeric)))

(it
  "keeps capture numbers stable across nested and uneven branch-reset alternatives"
  (let ((uneven (compile-regex "(?|(a)(b)|(c))(?|(d)|(e)(f))"))
        (nested (compile-regex "(?|(?|(a)|(b))|(c))\\1")))
    (expect-match-captures-cases
      ((scan uneven "abef") "abef" '("abef" "a" "b" "e" "f"))
      ((scan uneven "cd") "cd" '("cd" "c" nil "d" nil))
      ((scan nested "aa") "aa" '("aa" "a"))
      ((scan nested "bb") "bb" '("bb" "b"))
      ((scan nested "cc") "cc" '("cc" "c")))))

(it
  "covers PCRE named-reference spellings and bounded advanced anchors"
  (let ((bounded-text (format nil "a~C" #\Newline)))
    (expect-match-string-cases
      ((scan (compile-regex "(?<x>A)\\g{x}") "AA") "AA" "AA")
      ((scan (compile-regex "(?<x>A)\\k{x}") "AA") "AA" "AA"))
    (let ((short-mark (compile-regex "a(*:middle)b"))
          (bounded-dollar (compile-regex "a(*:middle)$"))
          (bounded-end (compile-regex "a(*:middle)\\z")))
      (expect-advanced-regex-p-cases short-mark bounded-dollar bounded-end)
      (expect (match-mark (scan short-mark "ab")) :to-equal "middle")
      (expect-match-span-cases
        ((scan bounded-dollar bounded-text :end 1) 0 1)
        ((scan bounded-end bounded-text :end 1) 0 1)))))

(it-advanced-public-scan-cases
  "covers advanced end anchors"
  (list (list "\\Z" "" 0 0 "")
        (list "\\Z" "a" 1 1 "")
        (list "\\Z" (format nil "a~C" #\Newline) 1 1 "")
        (list "\\Z" (format nil "a~C~C" #\Return #\Newline) 1 1 "")))

(it-advanced-public-scan-cases
  "covers Unicode grapheme clusters"
  (flet ((code-points (&rest code-points)
           (map-into (make-string (length code-points))
                     #'code-char
                     code-points)))
    (list (list "\\X"
                (code-points #x1100 #x1161 #x11a8)
                0
                3
                (code-points #x1100 #x1161 #x11a8))
          (list "\\X"
                (concatenate 'string (string (code-char #x600)) "a")
                0
                2
                (concatenate 'string (string (code-char #x600)) "a"))
          (list "\\X"
                (concatenate 'string "a" (string (code-char #x093e)))
                0
                2
                (concatenate 'string "a" (string (code-char #x093e))))
          (list "\\X"
                (code-points #x1f469 #x200d #x1f4bb)
                0
                3
                (code-points #x1f469 #x200d #x1f4bb))
          (list "\\X"
                (code-points #x1f1e6 #x1f1e7 #x1f1e8)
                0
                2
                (subseq (code-points #x1f1e6 #x1f1e7 #x1f1e8) 0 2))
          (list "\\X"
                (format nil "~C~C" #\Return #\Newline)
                0
                2
                (format nil "~C~C" #\Return #\Newline))
          (list "\\X"
                (code-points #x0915 #x094d #x0937)
                0
                3
                (code-points #x0915 #x094d #x0937)))))

(it
  "runs the exported advanced executor with implicit string and byte limits"
  (flet ((bytes (&rest values)
           (make-array (length values)
                       :element-type '(unsigned-byte 8)
                       :initial-contents values)))
    (let ((string-regex (compile-regex "(?=a)a"))
          (byte-regex (compile-byte-regex "\\Z")))
      (expect-match-span-cases
        ((cl-regex-kit:run-advanced-regex string-regex "za" :start 1) 1 2)
        ((cl-regex-kit:run-advanced-regex byte-regex (bytes #x61 #x0a)) 1 1)
        ((cl-regex-kit:run-advanced-regex
          byte-regex
          (bytes #x61 #x0d #x0a))
         1
         1)))))

(it
  "honors timeouts through the exported advanced executor"
  (let ((regex (compile-regex "(?=(a+)+$)"))
        (text (make-string 4000 :initial-element #\a)))
    (signals regex-timeout
      (cl-regex-kit:run-advanced-regex regex text :timeout 0.001))))

(it
  "signals advanced step limits through the public scan API"
  (let ((condition nil))
    (handler-case
        (scan (compile-regex "(?=a)a" :size-limit 1) "a")
      (advanced-regex-limit-error (caught)
        (setf condition caught)))
    (expect condition :to-be-truthy)
    (expect (and condition
                 (advanced-regex-limit-kind condition))
            :to-equal
            :steps)
    (expect (and condition
                 (advanced-regex-limit condition))
            :to-equal
            1)
    (expect (and condition
                 (> (advanced-regex-limit-used condition)
                    (advanced-regex-limit condition)))
            :to-be-truthy)))

(it
  "matches byte backreferences case-insensitively on the advanced path"
  (let* ((text (make-array 2
                           :element-type '(unsigned-byte 8)
                           :initial-contents '(#x61 #x41)))
         (result (byte-match "(?i)(?<x>A)\\k<x>" text)))
    (expect (and result
                 (list (match-start result)
                       (match-end result)))
            :to-equal
            '(0 2))))

(it
  "selects participating captures for duplicate named references"
  (let* ((backreference
           (compile-regex "(?J)\\A(?:(?<x>a)|(?<x>b)\\k<x>)\\z"))
         (conditional
           (compile-regex "(?J)\\A(?:(?<x>a)|(?<x>b))(?(x)c|d)\\z"))
         (relative
           (compile-regex "\\A(?<a>a)(?<b>b)\\g{-1}\\z")))
    (expect-advanced-regex-p-cases backreference conditional relative)
    (expect (scan backreference "bb") :to-be-truthy)
    (expect (scan backreference "ab") :to-be-null)
    (expect (scan conditional "bc") :to-be-truthy)
    (expect (scan conditional "bd") :to-be-null)
    (expect (scan relative "abb") :to-be-truthy)
    (expect (scan relative "aba") :to-be-null)))

(it-advanced-public-scan-cases
  "accepts Unicode capture names in advanced references"
  (let ((name (string (code-char #x00e9))))
    (list (list (format nil "(?<~A>a)\\g{~A}" name name) "aa" 0 2 "aa")
          (list (format nil "(?<~A>a)\\k<~A>" name name) "aa" 0 2 "aa")
          (list (format nil "(?<~A>a)(?&~A)" name name) "aa" 0 2 "aa")
          (list (format nil "(?<~A>a)(?(~A)b|c)" name name) "ab" 0 2 "ab"))))

(it
  "rejects unresolved advanced references during compilation"
  (expect-signals-cases
   regex-syntax-error
   (compile-regex "(?P=missing)")
   (compile-regex "(?&missing)")
   (compile-regex "(?P>missing)")
   (compile-regex "(?(missing)a|b)")
   (compile-regex "(?(R&missing)a|b)")
   (compile-regex "(?1)")))

(it
  "supports balancing groups on the advanced path"
  (let* ((balancing (compile-regex "(?<open>a)(?<-open>b)"))
         (repeated (compile-regex "(?<open>a)+(?<-open>b)+"))
         (underflow (compile-regex "(?<-open>b)"))
         (balancing-result (scan balancing "ab"))
         (repeated-result (scan repeated "aabb")))
    (dolist (regex (list balancing repeated underflow))
      (expect (regex-advanced-p regex) :to-be-truthy))
    (expect (and balancing-result
                 (list (match-start balancing-result)
                       (match-end balancing-result)))
            :to-equal
            (list 0 2))
    (expect (and repeated-result
                 (list (match-start repeated-result)
                       (match-end repeated-result)))
            :to-equal
            (list 0 4))
    (expect (scan underflow "b") :to-be-null)))

(it "runs PCRE2-style callouts"
  (let ((events nil))
    (let ((regex
            (compile-regex "(?C7)a"
                           :callout
                           (lambda (number tag position text)
                             (push (list number tag position text) events)
                             :continue))))
      (expect (scan regex "a") :to-be-truthy)
      (expect (cl-regex-kit:regex-callout regex) :to-be-truthy)
      (expect (nreverse events)
              :to-equal
              (list (list 7 nil 0 "a"))))
    (let ((regex
            (compile-regex "(?:(?C1)a|b)"
                           :callout
                           (lambda (number tag position text)
                             (declare (ignore number tag position text))
                             :fail))))
      (expect (scan regex "b") :to-be-truthy)
      (expect (scan regex "a") :to-be-null))
    (dolist (pattern
             (list "(?C\"tag\")a"
                   (format nil "(?C~Ctag~C)a" #\' #\')
                   "(?C^tag^)a"
                   "(?C%tag%)a"
                   "(?C#tag#)a"
                   "(?C$tag$)a"
                   "(?C{tag})a"))
      (let* ((events nil)
             (regex
               (compile-regex
                pattern
                :callout
                (lambda (number tag position text)
                  (push (list number tag position text) events)
                  :continue))))
        (expect (scan regex "a") :to-be-truthy)
        (expect (nreverse events)
                :to-equal
                (list (list 0 "tag" 0 "a")))))))

(it-evaluated-scan-range-cases
  "supports Unicode grapheme boundary anchors"
  "\\b{g}\\X\\b{g}"
  (list (list (format nil "a~C" (code-char #x301)) 0 2)))

(it-scan-range-cases
  "supports Unicode word and sentence boundary anchors"
  "\\b{wb}word\\b{wb}"
  (("word" 0 4)))

(it-scan-range-cases
  "supports Unicode sentence boundary anchors"
  "\\b{sb}B"
  (("A. B" 3 4)))

(it "rejects invalid Unicode sentence-boundary matches"
  (expect-falsy-cases
    (scan (compile-regex "\\b{sb}b") "A. b")
    (scan (compile-regex "\\r\\b{sb}\\n")
          (format nil "A~C~CB" #\Return #\Linefeed))))

(it-byte-scan-range-cases
  "supports Unicode grapheme boundaries on byte regexes"
  "(?u:\\b{g}\\X\\b{g})"
  (list
   (list (make-array 3
                     :element-type '(unsigned-byte 8)
                     :initial-contents '(97 204 129))
         0
         3)))

(it-byte-scan-range-cases
  "supports Unicode word boundaries on byte regexes"
  "(?u:\\b{wb}\\p{L}+\\b{wb})"
  (list
   (list (make-array 4
                     :element-type '(unsigned-byte 8)
                     :initial-contents '(97 195 169 33))
         0
         3)))

(it-byte-scan-range-cases
  "supports Unicode sentence boundaries on byte regexes"
  "(?u:\\b{sb}B)"
  (list
   (list (make-array 4
                     :element-type '(unsigned-byte 8)
                     :initial-contents '(65 46 32 66))
         3
         4)
   (list (make-array 2
                     :element-type '(unsigned-byte 8)
                     :initial-contents '(255 65))
         nil
         nil)))

(it-evaluated-scan-range-cases
  "evaluates grapheme boundaries at interior positions"
  "\\X\\b{g}b"
  (list (list (format nil "a~Cb" (code-char #x301)) 0 3)))
