;;;; t/api-test.lisp
(in-package #:cl-regex-kit/test)

(it
  "matches literals, classes, dot, and anchors through the public API"
  (expect
    (match-string (match "h.llo" "well hello!") "well hello!")
    :to-equal
    "hello")
  (expect (match-string (match "[^0-9]+" "42abc7") "42abc7") :to-equal "abc")
  (expect (match "^abc$" "xabc") :to-be-null)
  (expect (match-string (match "^abc$" "abc") "abc") :to-equal "abc"))

(it
  "matches octet vectors through the one-shot byte API"
  (let ((text (octets #xff #x41 #x42 #x80)))
    (let ((result (byte-match "(?-u:\\x41\\x42)" text :start 1 :end 3)))
      (expect (match-start result) :to-equal 1)
      (expect (match-end result) :to-equal 3)
      (expect (coerce (match-string result text) 'list) :to-equal '(65 66)))
    (expect (byte-match "(?-u:\\x41\\x42)" text :start 2) :to-be-null)))

(it
  "escapes arbitrary text using Rust-compatible meta-character quoting"
  (let ((literal
        (concatenate 'string ".+?()|[]{}^$\\#&-~ " (string #\Tab) (string #\Newline))))
    (expect
      (escape literal)
      :to-equal
      (concatenate
        'string
        "\\.\\+\\?\\(\\)\\|\\[\\]\\{\\}\\^\\$\\\\\\#\\&\\-\\~ "
        (string #\Tab)
        (string #\Newline)))
    (expect
      (match-string (match (escape literal) literal) literal)
      :to-equal
      literal)))

(it
  "rejects invalid literal regex macro invocations at expansion time"
  (expect-macroexpand-signals-cases
   error
   (cl-regex-kit:regex 42)
   (cl-regex-kit:regex "a" :case-insensitive)
   (cl-regex-kit:regex "a" case-insensitive t)
   (cl-regex-kit:regex "a" :case-insensitive dynamic-option)
   (cl-regex-kit:byte-regex 42)
   (cl-regex-kit:byte-regex "a" :case-insensitive)
   (cl-regex-kit:byte-regex "a" case-insensitive t)
   (cl-regex-kit:byte-regex "a" :case-insensitive dynamic-option)))

(it
  "rejects invalid literal regex-set macro invocations at expansion time"
  (expect-macroexpand-signals-cases
   error
   (cl-regex-kit:regex-set 42)
   (cl-regex-kit:regex-set "a" :case-insensitive)
   (cl-regex-kit:regex-set "a" case-insensitive t)
   (cl-regex-kit:regex-set "a" :case-insensitive dynamic-option)
   (cl-regex-kit:byte-regex-set 42)
   (cl-regex-kit:byte-regex-set "a" :case-insensitive)
   (cl-regex-kit:byte-regex-set "a" case-insensitive t)
   (cl-regex-kit:byte-regex-set "a" :case-insensitive dynamic-option)))

(it
  "scans compiled patterns and returns non-overlapping matches"
  (let* ((regex (compile-regex "\\d+"))
         (text "a1b22c333")
         (matches (all-matches regex text)))
    (expect (regex-p regex) :to-be-truthy)
    (expect
      (mapcar
        (lambda (result)
          (match-string result text))
        matches)
      :to-equal
      '("1" "22" "333"))
    (expect (mapcar #'match-start matches) :to-equal '(1 3 6))))

(it
  "uses zero as the default start position across matching APIs"
  (let* ((regex (compile-regex "a+"))
         (text "zaa"))
    (expect (match-start (scan regex text)) :to-equal 1)
    (expect (match-start (captures regex text)) :to-equal 1)
    (expect (shortest-match regex text) :to-equal 2)
    (expect (match-start (longest-match regex text)) :to-equal 1)
    (expect text :to-match-regex regex)
    (expect (match-start (match "a+" text)) :to-equal 1)
    (expect (match-string (full-match regex "aa") "aa") :to-equal "aa"))
  (let* ((regex (compile-byte-regex "(?-u:a+)"))
         (text
        (make-array 3 :element-type '(unsigned-byte 8) :initial-contents '(122 97 97))))
    (expect (match-start (byte-match "(?-u:a+)" text)) :to-equal 1)
    (expect (match-start (scan regex text)) :to-equal 1)))

(it
  "returns capture results through Rust-compatible capture entry points"
  (let* ((regex (compile-regex "(?<word>[a-z]+)-(?<number>[0-9]+)"))
         (text "--item-42 next"))
    (let ((result (captures regex text)))
      (expect
        (coerce (match-captures result text) 'list)
        :to-equal
        '("item-42" "item" "42"))
      (expect (match-group-string result "number" text) :to-equal "42"))
    (let ((result (captures-at regex text 3 :end 10)))
      (expect (match-start result) :to-equal 3)
      (expect (match-end result) :to-equal 9))
    (let ((result (scan-at regex text 3 :end 10)))
      (expect (match-start result) :to-equal 3)
      (expect (match-end result) :to-equal 9))
    (expect (is-match-at regex text 3 :end 10) :to-be-truthy)
    (expect (is-match-at regex text 10) :to-be-falsy)
    (expect (captures-at regex text 11) :to-be-null)
    (expect (scan-at regex text 11) :to-be-null)
    (signals type-error (captures-at regex text -1))
    (signals type-error (scan-at regex text -1))
    (signals type-error (is-match-at regex text -1)))
  (let* ((regex (compile-byte-regex "(?-u:(A)(B))"))
         (text (octets #xff 65 66 #x80))
         (result (captures-at regex text 1)))
    (expect (is-match-at regex text 1) :to-be-truthy)
    (expect
      (mapcar
        (lambda (capture)
          (coerce capture 'list))
        (coerce (match-captures result text) 'list))
      :to-equal
      '((65 66) (65) (66)))))

(it
  "reuses capture locations without retaining stale captures"
  (let* ((regex (compile-regex "(?<word>[a-z]+)(?:-(?<number>\\d+))?"))
         (locations (regex-capture-locations regex)))
    (multiple-value-bind (start end) (scan-captures-into regex locations "--item-42")
      (expect start :to-equal 2)
      (expect end :to-equal 9))
    (multiple-value-bind (start end) (scan-captures-into-at regex locations "--item-42" 2 :end 9)
      (expect start :to-equal 2)
      (expect end :to-equal 9))
    (expect (capture-locations-count locations) :to-equal 3)
    (expect (capture-location-start locations 0) :to-equal 2)
    (expect (capture-location-end locations 0) :to-equal 9)
    (expect (capture-location-start locations 1) :to-equal 2)
    (expect (capture-location-end locations 1) :to-equal 6)
    (expect (capture-location-start locations 2) :to-equal 7)
    (expect (capture-location-end locations 2) :to-equal 9)
    (multiple-value-bind (start end) (scan-captures-into regex locations "next")
      (expect start :to-equal 0)
      (expect end :to-equal 4))
    (expect (capture-location-start locations 2) :to-be-null)
    (multiple-value-bind (start end) (scan-captures-into regex locations "123")
      (declare (ignore end))
      (expect start :to-be-null))
    (expect (capture-location-start locations 0) :to-be-null)
    (expect (capture-location-end locations 1) :to-be-null)
    (signals type-error (capture-location-start locations 3))
    (signals type-error (scan-captures-into (compile-regex "(a)") locations "a"))
    (signals type-error (scan-captures-into-at regex locations "item" -1)))
  (let* ((regex (compile-byte-regex "(a)(b)?" :unicode nil))
         (locations (regex-capture-locations regex))
         (text (make-array 1 :element-type '(unsigned-byte 8) :initial-contents '(97))))
    (multiple-value-bind (start end) (scan-captures-into regex locations text)
      (expect start :to-equal 0)
      (expect end :to-equal 1))
    (expect (capture-location-start locations 1) :to-equal 0)
    (expect (capture-location-end locations 2) :to-be-null)
    (multiple-value-bind (start end) (scan-captures-into-at regex locations text 0)
      (expect start :to-equal 0)
      (expect end :to-equal 1))))

(it
  "selects RE2-style leftmost-longest matches without changing scan"
  (let* ((regex (compile-regex "(?<unit>a|aa)"))
         (text "xaab"))
    (expect (match-string (scan regex text) text) :to-equal "a")
    (let ((result (longest-match regex text)))
      (expect (match-string result text) :to-equal "aa")
      (expect (match-start result) :to-equal 1)
      (expect (match-group-string result "unit" text) :to-equal "aa")))
  (let ((regex (compile-regex "a+|b+")))
    (expect (match-string (longest-match regex "aa-bbbb") "aa-bbbb") :to-equal "aa")
    (expect
      (match-string (longest-match regex "xaaa" :start 2) "xaaa")
      :to-equal
      "aa"))
  (let ((regex (compile-regex "a*|b+")))
    (expect (match-string (longest-match regex "b") "b") :to-equal "b"))
  (let* ((regex (compile-regex "(?<preferred>a)|(?<other>a)"))
         (result (longest-match regex "a")))
    (expect (match-group-string result "preferred" "a") :to-equal "a")
    (expect (match-group-string result "other" "a") :to-be-null))
  (let* ((regex (compile-byte-regex "(?-u:\\x80+)"))
         (text
        (make-array 3 :element-type '(unsigned-byte 8) :initial-contents '(128 128 1))))
    (expect
      (coerce (match-string (longest-match regex text) text) 'list)
      :to-equal
      '(128 128))))

(it
  "supports bounded repetition, non-capturing groups, and whitespace shorthands"
  (expect
    (match-string (match "(?:ab){2,3}" "abababx") "abababx")
    :to-equal
    "ababab")
  (expect
    (match-string (match "\\w+" "--abc_42!") "--abc_42!")
    :to-equal
    "abc_42")
  (let ((text (format nil "x~C~Cy" #\Tab #\Newline)))
    (expect
      (match-string (match "\\s+" text) text)
      :to-equal
      (format nil "~C~C" #\Tab #\Newline))))

(it
  "advances through zero-length matches"
  (let ((matches (all-matches (compile-regex "a*") "b")))
    (expect (mapcar #'match-start matches) :to-equal '(0 1))))

(it
  "supports inline flags and hexadecimal escapes"
  (expect (match-string (match "(?i)abc" "--AbC--") "--AbC--") :to-equal "AbC")
  (expect (match-string (match "a(?i:bc)d" "aBCd") "aBCd") :to-equal "aBCd")
  (expect
    (match-string
      (match "(?s)a.b" (format nil "a~Cb" #\Newline))
      (format nil "a~Cb" #\Newline))
    :to-equal
    (format nil "a~Cb" #\Newline))
  (expect
    (match-string
      (match "(?m)^bar$" (format nil "foo~Cbar~Cbaz" #\Newline #\Newline))
      (format nil "foo~Cbar~Cbaz" #\Newline #\Newline))
    :to-equal
    "bar")
  (expect (match-string (match "(?U)a+" "aaaa") "aaaa") :to-equal "a")
  (expect (match-string (match "\\x41\\u{42}" "AB") "AB") :to-equal "AB")
  (expect (match-string (match "\\x{1f600}" "😀") "😀") :to-equal "😀")
  (expect (match-string (match "\\U00000043\\U{44}" "CD") "CD") :to-equal "CD")
  (expect
    (match-string (match "[\\U00000045\\U{46}]+" "xEFy") "xEFy")
    :to-equal
    "EF")
  (signals regex-syntax-error (compile-regex "\\U00110000"))
  (let ((kelvin (string (code-char #x212A)))
        (long-s (string (code-char #x017F)))
        (capital-sharp-s (string (code-char #x1E9E)))
        (sharp-s (string (code-char #x00DF)))
        (dotted-i (string (code-char #x0130)))
        (ff-ligature (string (code-char #xFB00)))
        (final-sigma (string (code-char #x03C2)))
        (sigma (code-char #x03C3)))
    (expect (match-string (match "(?i)k" kelvin) kelvin) :to-equal kelvin)
    (expect (match-string (match "(?i)s" long-s) long-s) :to-equal long-s)
    (expect
      (match-string (match (format nil "(?i)~C" sigma) final-sigma) final-sigma)
      :to-equal
      final-sigma)
    (expect (match-string (match "(?i)[k]" kelvin) kelvin) :to-equal kelvin)
    (expect
      (match-string (match "(?i)ß" capital-sharp-s) capital-sharp-s)
      :to-equal
      capital-sharp-s)
    (expect
      (match-string (match "(?i)[ß]" capital-sharp-s) capital-sharp-s)
      :to-equal
      capital-sharp-s)
    (expect (match "(?i)[f]" ff-ligature) :to-be-null)
    (expect (match "(?i)[i]" dotted-i) :to-be-null)
    (expect (match "(?i)[s]" sharp-s) :to-be-null)
    (expect
      (match-string (match (format nil "(?i)[~C]" sigma) final-sigma) final-sigma)
      :to-equal
      final-sigma)))

(it
  "recognizes public match result and capture location values"
  (let* ((regex (compile-regex "(a)"))
         (result (scan regex "a"))
         (locations (regex-capture-locations regex)))
    (expect (match-result-p result) :to-be-truthy)
    (expect (match-result-p nil) :to-be nil)
    (expect (match-result-p "a") :to-be nil)
    (expect (capture-locations-p locations) :to-be-truthy)
    (expect (capture-locations-p nil) :to-be nil)
    (expect (capture-locations-p result) :to-be nil)))

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

(it
  "covers cut and start anchors through public scan"
  (let ((cut (compile-regex "a\\Kb"))
        (start (compile-regex "\\Gabc")))
    (expect-advanced-regex-p-cases cut start)
    (expect-match-span-cases
      ((scan cut "ab") 1 2)
      ((scan start "zabc" :start 1) 1 4))
    (expect-match-string-cases
      ((scan cut "ab") "ab" "b")
      ((scan start "zabc" :start 1) "zabc" "abc"))))

(it
  "covers grapheme clusters through public scan"
  (let ((grapheme (compile-regex "\\X"))
        (grapheme-text (format nil "a~C" (code-char #x301)))
        (indic-conjunct-text
          (format nil "~C~C~C"
                  (code-char #x915)
                  (code-char #x94D)
                  (code-char #x915))))
    (expect-advanced-regex-p-cases grapheme)
    (expect-match-span-cases
      ((scan grapheme grapheme-text) 0 2)
      ((scan grapheme indic-conjunct-text) 0 3))
    (expect-match-string-cases
      ((scan grapheme grapheme-text) grapheme-text grapheme-text)
      ((scan grapheme indic-conjunct-text)
       indic-conjunct-text
       indic-conjunct-text))))

(it
  "covers branch-reset, conditionals, definitions, and recursion"
  (let ((branch-reset (compile-regex "(?|(a)|(b))\\1"))
        (conditional (compile-regex "(a)?(?(1)b|c)"))
        (definition (compile-regex "(?(DEFINE)(?<word>[a-z]+))(?&word)"))
        (recursive (compile-regex "(?<paren>\\((?:[^()]|(?&paren))*\\))")))
    (expect-advanced-regex-p-cases
      branch-reset
      conditional
      definition
      recursive)
    (expect-match-string-cases
      ((scan branch-reset "bb") "bb" "bb")
      ((scan conditional "c") "c" "c")
      ((scan conditional "ab") "ab" "ab")
      ((scan definition "abc") "abc" "abc")
      ((scan recursive "(a(b))") "(a(b))" "(a(b))"))))

(it
  "validates direct recursive and numeric subroutine targets"
  (let ((recursive (compile-regex "(?R)"))
        (zero (compile-regex "(?0)"))
        (numeric (compile-regex "(?<item>a)(?1)")))
    (dolist (regex (list recursive zero numeric))
      (expect (regex-advanced-p regex) :to-be-truthy))))

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
    (dolist (case '(("(?<x>A)\\g{x}" "AA" "AA")
                    ("(?<x>A)\\k{x}" "AA" "AA")))
      (destructuring-bind (pattern text expected) case
        (let ((regex (compile-regex pattern)))
          (expect-advanced-regex-p-cases regex)
          (expect (and (scan regex text)
                       (match-string (scan regex text) text))
                  :to-equal
                  expected))))
    (let ((short-mark (compile-regex "a(*:middle)b"))
          (bounded-dollar (compile-regex "a(*:middle)$"))
          (bounded-end (compile-regex "a(*:middle)\\z")))
      (expect-advanced-regex-p-cases short-mark bounded-dollar bounded-end)
      (expect (match-mark (scan short-mark "ab")) :to-equal "middle")
      (expect-match-span-cases
        ((scan bounded-dollar bounded-text :end 1) 0 1)
        ((scan bounded-end bounded-text :end 1) 0 1)))))

(it
  "covers advanced end anchors and Unicode grapheme clusters"
  (let ((end-anchor (compile-regex "\\Z"))
        (grapheme (compile-regex "\\X")))
    (flet ((span (regex text)
             (let ((result (scan regex text)))
               (and result
                    (list (match-start result)
                          (match-end result)))))
           (code-points (&rest code-points)
             (map-into (make-string (length code-points))
                       #'code-char
                       code-points)))
      (dolist (regex (list end-anchor grapheme))
        (expect (regex-advanced-p regex) :to-be-truthy))
      (dolist (case (list (list "" 0 0)
                          (list "a" 1 1)
                          (list (format nil "a~C" #\Newline) 1 1)
                          (list (format nil "a~C~C" #\Return #\Newline) 1 1)))
        (destructuring-bind (text expected-start expected-end) case
          (expect (span end-anchor text)
                  :to-equal
                  (list expected-start expected-end))))
      (dolist (case
                (list
                 (list (code-points #x1100 #x1161 #x11a8) 3)
                 (list (concatenate 'string (string (code-char #x600)) "a") 2)
                 (list (concatenate 'string "a" (string (code-char #x093e))) 2)
                 (list (code-points #x1f469 #x200d #x1f4bb) 3)
                 (list (code-points #x1f1e6 #x1f1e7 #x1f1e8) 2)
                 (list (format nil "~C~C" #\Return #\Newline) 2)
                 (list (code-points #x0915 #x094d #x0937) 3)))
        (destructuring-bind (text expected-end) case
          (expect (span grapheme text)
                  :to-equal
                  (list 0 expected-end)))))))

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
    (dolist (regex (list backreference conditional relative))
      (expect (regex-advanced-p regex) :to-be-truthy))
    (expect (scan backreference "bb") :to-be-truthy)
    (expect (scan backreference "ab") :to-be-null)
    (expect (scan conditional "bc") :to-be-truthy)
    (expect (scan conditional "bd") :to-be-null)
    (expect (scan relative "abb") :to-be-truthy)
    (expect (scan relative "aba") :to-be-null)))

(it
  "accepts Unicode capture names in advanced references"
  (let ((name (string (code-char #x00e9))))
    (dolist (case `((,(format nil "(?<~A>a)\\g{~A}" name name) "aa" 0 2)
                    (,(format nil "(?<~A>a)\\k<~A>" name name) "aa" 0 2)
                    (,(format nil "(?<~A>a)(?&~A)" name name) "aa" 0 2)
                    (,(format nil "(?<~A>a)(?(~A)b|c)" name name) "ab" 0 2)))
      (destructuring-bind (pattern text expected-start expected-end) case
        (let ((regex (compile-regex pattern)))
          (expect-advanced-regex-config-cases regex)
          (expect-match-span-cases
            ((scan regex text) expected-start expected-end)))))))

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

(it
 "reports regex-set cardinality without copying its source patterns"
 (let ((empty (compile-regex-set '() :size-limit 1))
       (set (compile-regex-set '("cat" "dog")))
       (byte-set (compile-byte-regex-set '("A"))))
   (expect (regex-set-count empty) :to-equal 0)
   (expect (regex-set-empty-p empty) :to-be-truthy)
   (expect (regex-set-count set) :to-equal 2)
   (expect (regex-set-empty-p set) :to-be nil)
   (expect (regex-set-count byte-set) :to-equal 1)
   (expect (regex-set-empty-p byte-set) :to-be nil)
   (signals type-error (regex-set-count nil))
   (signals type-error (regex-set-empty-p nil))))

(it
 "validates compiler options for empty regex sets"
 (expect-signals-for-functions
  error
  (list #'compile-regex-set #'compile-byte-regex-set)
  (funcall fn '() :unknown-option t))
 (expect-signals-for-functions
  type-error
  (list #'compile-regex-set #'compile-byte-regex-set)
  (funcall fn '() :case-insensitive :enabled)
  (funcall fn '() :literal :enabled)
  (funcall fn '() :nest-limit -1)
  (funcall fn '() :size-limit 0)
  (funcall fn '() :line-terminator "newline")
  (funcall fn '() :line-terminator (code-char #x80))))

(it
 "compiles immutable regex sets and reports every matching pattern index"
 (expect-regex-set-cases
  ((compile-regex-set '("cat" "dog" "\\d+")) "dog42" 0 '(1 2))
  ((compile-regex-set '("cat" "dog" "\\d+")) "bird" 0 nil)
  ((compile-regex-set '("a" "a" "^z")) "a" 0 '(0 1))
  ((compile-regex-set '("a")) "za" 2 nil)
  ((compile-regex-set '()) "anything" 0 nil))
 (let* ((source (vector "cat" "dog"))
        (set (compile-regex-set source)))
   (setf (aref source 0) "bird")
   (expect (equalp (regex-set-patterns set) #("cat" "dog")) :to-be-truthy)
   (setf (aref (regex-set-patterns set) 0) "bird")
   (expect (equalp (regex-set-patterns set) #("cat" "dog")) :to-be-truthy))
 (dolist (compiler (list #'compile-regex #'compile-byte-regex))
   (let* ((source (copy-seq "cat"))
          (compiled (funcall compiler source)))
     (setf (char source 0) #\b)
     (expect (cl-regex-kit:regex-source compiled) :to-equal "cat")
     (let ((exposed (cl-regex-kit:regex-source compiled)))
       (setf (char exposed 0) #\r)
       (expect (cl-regex-kit:regex-source compiled) :to-equal "cat"))))
 (dolist (compiler (list #'compile-regex-set #'compile-byte-regex-set))
   (let* ((source (vector (copy-seq "cat") (copy-seq "dog")))
          (set (funcall compiler source)))
     (setf (char (aref source 0) 0) #\b)
     (expect (equalp (regex-set-patterns set) #("cat" "dog")) :to-be-truthy)
     (let ((exposed (regex-set-patterns set)))
       (setf (char (aref exposed 0) 0) #\r)
       (expect (equalp (regex-set-patterns set) #("cat" "dog")) :to-be-truthy))))
 (let ((set (regex-set "cat" "dog")))
   (expect (regex-set-matches set "a dog") :to-equal '(1)))
 (let ((set (regex-set "cat" :case-insensitive t)))
   (expect (regex-set-matches set "CAT") :to-equal '(0)))
 (signals error (macroexpand-1 '(regex-set "cat" :case-insensitive enabled)))
 (let ((set
        (compile-regex-set
         '("^right$")
         :multi-line
         t
         :crlf
         t
         :line-terminator
         #\;))
       (crlf-text
        (format
         nil
         "left~C~Cright~C~Ctail"
         #\Return
         #\Newline
         #\Return
         #\Newline)))
   (expect (regex-set-matches set crlf-text) :to-equal '(0))))

(it
 "keeps merged regex-set execution equivalent to member regex scans"
 (expect-regex-set-equivalent-cases
  ('("(?<letter>a)" "b") "a" 0)
  ('("a*" "^$" "\\A\\z") "" 0)
  ('("(?i)cat" "\\p{Greek}+") "Cat α" 0)
  ('("(?i)k" "\\w+" "(?s:.)") "K\r\n" 0)
  ('("\\Afoo\\z" "o+") "foo" 1)
  ('("(?m)^foo$" "bar") "x\nfoo\ny" 0)
  ('("\\b{start-half}\\w+" "\\b{end-half}\\w+") "α" 0)
  ('("a" "a") "a" 0)))

(it
 "keeps larger merged regex sets equivalent to member regex scans"
 (let ((patterns
        (loop for index below 32
              collect (format nil "(?:item~D|code~D)(?:-x)?" index index))))
   (expect-regex-set-equivalent-cases
    (patterns "prefix code17-x middle item3 suffix" 0))))

(it
 "validates regex-set parallel compilation boundaries"
 (expect-signals-for-values
  (value '(0 9))
  type-error
  (let ((cl-regex-kit::*nfa-merge-parallelism-override* value))
    (cl-regex-kit::nfa-merge-parallelism 2 65536))
  (let ((cl-regex-kit::*regex-set-compile-parallelism-override* value))
    (cl-regex-kit::regex-set-compile-parallelism 2 65536)))
 (let ((cl-regex-kit::*nfa-merge-parallelism-override* 2)
       (cl-regex-kit::*regex-set-compile-parallelism-override* 2))
   (let ((set (compile-regex-set '("a" "b"))))
     (expect (regex-set-matches set "b") :to-equal '(1))))
 (let ((calls 0))
   (signals
    regex-syntax-error
    (cl-regex-kit::compile-regex-set-with
     '("a" "b")
     '(:size-limit 1)
     (lambda (pattern &rest options)
       (declare (ignore options))
       (incf calls)
       (compile-regex pattern))
     nil))
   (unless (zerop calls)
     (error "Size-limit rejection compiled ~D member patterns" calls))))

(it
 "selects default regex-set compilation parallelism from work size"
 (let ((cl-regex-kit::*regex-set-compile-parallelism-override* nil))
   (expect
    (cl-regex-kit::regex-set-compile-parallelism 255 8192)
    :to-equal
    1)
   (expect
    (cl-regex-kit::regex-set-compile-parallelism 256 8192)
    :to-equal
    cl-regex-kit::+regex-set-max-parallelism+)
   (expect
    (cl-regex-kit::regex-set-compile-parallelism 256 8191)
    :to-equal
    1)))

(it
 "preserves regex-set order across parallel compile batches and NFA relocation"
 (let* ((patterns
         (append
          (loop for index below 15
                collect (format nil "(?:item~D|code~D)(?:-x)?" index index))
          (list
           "(?:item3|code3)(?:-x)?"
           "(?:item3|code3)(?:-x)?")))
        (text "prefix code7-x middle item3 suffix"))
   (let ((cl-regex-kit::*nfa-merge-parallelism-override* 2)
         (cl-regex-kit::*regex-set-compile-parallelism-override* 2))
     (let ((set (compile-regex-set patterns)))
       (expect (regex-set-count set) :to-equal 17)
       (expect (regex-set-matches set text) :to-equal '(3 7 15 16))))))

(it
 "matches CRLF as one line break in regex-set execution"
 (let* ((text (format nil "~C~C" #\Return #\Newline))
        (set (compile-regex-set (list "\\R" "a")))
        (byte-text
         (make-array 2
                     :element-type '(unsigned-byte 8)
                     :initial-contents (list #x0d #x0a)))
        (byte-set (compile-byte-regex-set (list "\\R" "a"))))
   (expect (regex-set-matches set text) :to-equal '(0))
   (expect (regex-set-matches byte-set byte-text) :to-equal '(0))))
