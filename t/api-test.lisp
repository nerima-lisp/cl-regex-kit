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
  (flet ((octets (&rest values)
           (make-array
          (length values)
          :element-type
          '(unsigned-byte 8)
          :initial-contents
          values)))
    (let ((text (octets #xff #x41 #x42 #x80)))
      (let ((result (byte-match "(?-u:\\x41\\x42)" text :start 1 :end 3)))
        (expect (match-start result) :to-equal 1)
        (expect (match-end result) :to-equal 3)
        (expect (coerce (match-string result text) 'list) :to-equal '(65 66)))
      (expect (byte-match "(?-u:\\x41\\x42)" text :start 2) :to-be-null))))

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
  (dolist (form
      '((cl-regex-kit:regex 42)
        (cl-regex-kit:regex "a" :case-insensitive)
        (cl-regex-kit:regex "a" case-insensitive t)
        (cl-regex-kit:regex "a" :case-insensitive dynamic-option)
        (cl-regex-kit:byte-regex 42)
        (cl-regex-kit:byte-regex "a" :case-insensitive)
        (cl-regex-kit:byte-regex "a" case-insensitive t)
        (cl-regex-kit:byte-regex "a" :case-insensitive dynamic-option)))
    (signals error (macroexpand-1 form))))

(it
  "rejects invalid literal regex-set macro invocations at expansion time"
  (dolist (form
      '((cl-regex-kit:regex-set 42)
        (cl-regex-kit:regex-set "a" :case-insensitive)
        (cl-regex-kit:regex-set "a" case-insensitive t)
        (cl-regex-kit:regex-set "a" :case-insensitive dynamic-option)
        (cl-regex-kit:byte-regex-set 42)
        (cl-regex-kit:byte-regex-set "a" :case-insensitive)
        (cl-regex-kit:byte-regex-set "a" case-insensitive t)
        (cl-regex-kit:byte-regex-set "a" :case-insensitive dynamic-option)))
    (signals error (macroexpand-1 form))))

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
  (flet ((octets (&rest values)
           (make-array
          (length values)
          :element-type
          '(unsigned-byte 8)
          :initial-contents
          values)))
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
        '((65 66) (65) (66))))))

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
  "executes advanced ordered-backtracking patterns through public scan"
  (let* ((lookahead (compile-regex "(?=a)a"))
         (lookbehind (compile-regex "(?<=a)b"))
         (possessive (compile-regex "a++"))
         (bounded-possessive (compile-regex "a{2,2}+b"))
         (possessive-failure (compile-regex "a++a"))
         (atomic (compile-regex "(?>a|ab)c"))
         (quoted-subroutine (compile-regex "(?<word>a)\\g'word'"))
         (lookahead-result (scan lookahead "a"))
         (lookbehind-result (scan lookbehind "ab"))
         (possessive-result (scan possessive "aa"))
         (bounded-possessive-result (scan bounded-possessive "aab"))
         (atomic-result (scan atomic "ac"))
         (quoted-subroutine-result (scan quoted-subroutine "aa")))
    (dolist (regex (list lookahead
                         lookbehind
                         possessive
                         bounded-possessive
                         atomic
                         quoted-subroutine))
      (expect (regex-advanced-p regex) :to-be-truthy)
      (expect (regex-advanced-step-limit regex) :to-be-truthy)
      (expect (regex-advanced-nest-limit regex) :to-be-truthy)
      (expect (regex-never-newline-p regex) :to-be nil))
    (expect (and lookahead-result
                 (list (match-start lookahead-result)
                       (match-end lookahead-result)))
            :to-equal
            (list 0 1))
    (expect (and lookbehind-result
                 (list (match-start lookbehind-result)
                       (match-end lookbehind-result)))
            :to-equal
            (list 1 2))
    (expect (and possessive-result
                 (list (match-start possessive-result)
                       (match-end possessive-result)))
            :to-equal
            (list 0 2))
    (expect (and bounded-possessive-result
                 (list (match-start bounded-possessive-result)
                       (match-end bounded-possessive-result)))
            :to-equal
            (list 0 3))
    (expect (scan possessive-failure "aa") :to-be-null)
    (expect (and atomic-result
                 (list (match-start atomic-result)
                       (match-end atomic-result)))
            :to-equal
            (list 0 2))
    (expect (and quoted-subroutine-result
                 (list (match-start quoted-subroutine-result)
                       (match-end quoted-subroutine-result)))
            :to-equal
            (list 0 2))
    (expect (scan atomic "abc") :to-be-null)))
(it
  "keeps negative lookarounds honest on both successful and failing paths"
  (let* ((negative-lookahead-success (scan (compile-regex "(?!a)b") "b"))
         (negative-lookahead-failure (scan (compile-regex "(?!a)a") "a"))
         (negative-lookbehind-success (scan (compile-regex "(?<!a)b") "b"))
         (negative-lookbehind-failure (scan (compile-regex "(?<!a)b") "ab")))
    (expect (and negative-lookahead-success
                 (list (match-start negative-lookahead-success)
                       (match-end negative-lookahead-success)))
            :to-equal
            '(0 1))
    (expect negative-lookahead-failure :to-be-null)
    (expect (and negative-lookbehind-success
                 (list (match-start negative-lookbehind-success)
                       (match-end negative-lookbehind-success)))
            :to-equal
            '(0 1))
    (expect negative-lookbehind-failure :to-be-null)))
(it "annotates only proven fixed-width lookbehinds" (labels ((width (pattern &key byte-mode) (let ((ast (if byte-mode (cl-regex-kit::regex-ast (compile-byte-regex pattern)) (cl-regex-kit::regex-ast (compile-regex pattern))))) (cl-regex-kit::assertion-node-fixed-length ast)))) (expect (width "(?<=ab)") :to-equal 2) (expect (width "(?<=a|bc)") :to-be nil) (expect (width "(?<=a{2})") :to-equal 2) (expect (width "(?<=a+)") :to-be nil) (expect (width "(?<=é)" :byte-mode t) :to-equal 2) (expect (width "(?<=[é])" :byte-mode t) :to-be nil)))
(it
  "executes variable-length lookbehinds and preserves their captures"
  (let* ((positive (scan (compile-regex "(?<=a+)b") "aaab"))
         (positive-alternation (scan (compile-regex "(?<=a|bc)d") "bcd"))
         (negative (scan (compile-regex "(?<!a+)b") "b"))
         (positive-miss (scan (compile-regex "(?<=a+)b") "b"))
         (negative-miss (scan (compile-regex "(?<!a+)b") "aaab"))
         (captured (scan (compile-regex "(?<=(a+))b") "aaab")))
    (expect (and positive
                 (list (match-start positive)
                       (match-end positive)))
            :to-equal
            '(3 4))
    (expect (and positive-alternation
                 (list (match-start positive-alternation)
                       (match-end positive-alternation)))
            :to-equal
            '(2 3))
    (expect (and negative
                 (list (match-start negative)
                       (match-end negative)))
            :to-equal
            '(0 1))
    (expect positive-miss :to-be-null)
    (expect negative-miss :to-be-null)
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
    (expect (regex-advanced-p regex) :to-be-truthy)
    (expect (match-string result "ab") :to-equal "ab")
    (expect (match-mark result) :to-equal "middle")))
(it
  "applies advanced result selection and control verbs"
  (let* ((selection (compile-regex "(?=a)(?:a|aa)"))
         (fail (compile-regex "a(*FAIL)|b"))
         (accept (compile-regex "a(*ACCEPT)b"))
         (prune (compile-regex "a(*PRUNE)b|ac"))
         (then (compile-regex "a(*THEN)b|ac"))
         (bare-skip (compile-regex "ab(*SKIP)(*FAIL)|b"))
         (named-skip
           (compile-regex
            "ab(*MARK:target)c(*SKIP:target)(*FAIL)|b|c"))
         (multiple-names
           (compile-regex
            "ab(*MARK:A)c(*MARK:B)d(*SKIP:A)(*FAIL)|b|c|d"))
         (missing-named-skip
           (compile-regex "ab(*SKIP:missing)(*FAIL)|b"))
         (longest (longest-match selection "aa")))
    (dolist (regex
             (list selection fail accept prune then bare-skip named-skip
                   multiple-names missing-named-skip))
      (expect (regex-advanced-p regex) :to-be-truthy))
    (expect (shortest-match selection "aa") :to-equal 1)
    (expect (and longest
                 (list (match-start longest)
                       (match-end longest)
                       (match-string longest "aa")))
            :to-equal
            (list 0 2 "aa"))
    (expect (match-string (scan fail "b") "b") :to-equal "b")
    (expect (match-string (scan accept "ab") "ab") :to-equal "a")
    (expect (scan prune "ac") :to-be-null)
    (expect (scan bare-skip "ab") :to-be-null)
    (let ((result (scan named-skip "abc")))
      (expect (and result
                   (list (match-start result)
                         (match-end result)
                         (match-string result "abc")))
              :to-equal
              (list 2 3 "c")))
    (let ((result (scan multiple-names "abcd")))
      (expect (match-string result "abcd") :to-equal "c"))
    (let ((result (scan missing-named-skip "ab")))
      (expect (match-string result "ab") :to-equal "b"))
    (let ((result (scan then "ac")))
      (expect result :to-be-truthy)
      (expect (match-string result "ac") :to-equal "ac"))))
(it
  "covers advanced syntax families through public scan"
  (let* ((cut (compile-regex "a\\Kb"))
         (start (compile-regex "\\Gabc"))
         (grapheme (compile-regex "\\X"))
         (branch-reset (compile-regex "(?|(a)|(b))\\1"))
         (conditional (compile-regex "(a)?(?(1)b|c)"))
         (definition (compile-regex "(?(DEFINE)(?<word>[a-z]+))(?&word)"))
         (recursive (compile-regex "(?<paren>\\((?:[^()]|(?&paren))*\\))"))
         (cut-result (scan cut "ab"))
         (start-result (scan start "zabc" :start 1))
         (grapheme-text (format nil "a~C" (code-char #x301)))
         (grapheme-result (scan grapheme grapheme-text))
         (indic-conjunct-text
           (format nil "~C~C~C"
                   (code-char #x915)
                   (code-char #x94D)
                   (code-char #x915)))
         (indic-conjunct-result (scan grapheme indic-conjunct-text))
         (branch-result (scan branch-reset "bb"))
         (conditional-result (scan conditional "c"))
         (conditional-true-result (scan conditional "ab"))
         (definition-result (scan definition "abc"))
         (recursive-result (scan recursive "(a(b))")))
    (dolist (regex (list cut start grapheme branch-reset conditional definition recursive))
      (expect (regex-advanced-p regex) :to-be-truthy))
    (expect (and cut-result
                 (list (match-start cut-result)
                       (match-end cut-result)
                       (match-string cut-result "ab")))
            :to-equal
            (list 1 2 "b"))
    (expect (and start-result
                 (list (match-start start-result)
                       (match-end start-result)
                       (match-string start-result "zabc")))
            :to-equal
            (list 1 4 "abc"))
    (expect (and grapheme-result
                 (list (match-start grapheme-result)
                       (match-end grapheme-result)
                       (match-string grapheme-result grapheme-text)))
            :to-equal
            (list 0 2 grapheme-text))
    (expect (and indic-conjunct-result
                 (list (match-start indic-conjunct-result)
                       (match-end indic-conjunct-result)
                       (match-string indic-conjunct-result indic-conjunct-text)))
            :to-equal
            (list 0 3 indic-conjunct-text))
    (expect (match-string branch-result "bb") :to-equal "bb")
    (expect (match-string conditional-result "c") :to-equal "c")
    (expect (match-string conditional-true-result "ab") :to-equal "ab")
    (expect (match-string definition-result "abc") :to-equal "abc")
    (expect (match-string recursive-result "(a(b))") :to-equal "(a(b))")))
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
    (dolist (case
              '(("abef" ("abef" "a" "b" "e" "f"))
                ("cd" ("cd" "c" nil "d" nil))))
      (destructuring-bind (text expected) case
        (let ((result (scan uneven text)))
          (expect (and result (coerce (match-captures result text) 'list))
                  :to-equal
                  expected))))
    (dolist (case
              '(("aa" ("aa" "a"))
                ("bb" ("bb" "b"))
                ("cc" ("cc" "c"))))
      (destructuring-bind (text expected) case
        (let ((result (scan nested text)))
          (expect (and result (coerce (match-captures result text) 'list))
                  :to-equal
                  expected))))))
(it
  "covers PCRE named-reference spellings and bounded advanced anchors"
  (let* ((g-brace (compile-regex "(?<x>A)\\g{x}"))
         (k-brace (compile-regex "(?<x>A)\\k{x}"))
         (short-mark (compile-regex "a(*:middle)b"))
         (bounded-dollar (compile-regex "a(*:middle)$"))
         (bounded-end (compile-regex "a(*:middle)\\z"))
         (bounded-text (format nil "a~C" #\Newline))
         (g-result (scan g-brace "AA"))
         (k-result (scan k-brace "AA"))
         (mark-result (scan short-mark "ab"))
         (dollar-result (scan bounded-dollar bounded-text :end 1))
         (end-result (scan bounded-end bounded-text :end 1)))
    (dolist (regex (list g-brace k-brace short-mark bounded-dollar bounded-end))
      (expect (regex-advanced-p regex) :to-be-truthy))
    (expect (and g-result (match-string g-result "AA")) :to-equal "AA")
    (expect (and k-result (match-string k-result "AA")) :to-equal "AA")
    (expect (match-mark mark-result) :to-equal "middle")
    (expect (and dollar-result
                 (list (match-start dollar-result)
                       (match-end dollar-result)))
            :to-equal
            (list 0 1))
    (expect (and end-result
                 (list (match-start end-result)
                       (match-end end-result)))
            :to-equal
            (list 0 1))))
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
    (let* ((string-regex (compile-regex "(?=a)a"))
           (string-result
             (cl-regex-kit:run-advanced-regex string-regex "za" :start 1))
           (byte-regex (compile-byte-regex "\\Z"))
           (byte-lf-result
             (cl-regex-kit:run-advanced-regex byte-regex (bytes #x61 #x0a)))
           (byte-crlf-result
             (cl-regex-kit:run-advanced-regex
              byte-regex
              (bytes #x61 #x0d #x0a))))
      (expect (and string-result
                   (list (match-start string-result)
                         (match-end string-result)))
              :to-equal
              '(1 2))
      (expect (and byte-lf-result
                   (list (match-start byte-lf-result)
                         (match-end byte-lf-result)))
              :to-equal
              '(1 1))
      (expect (and byte-crlf-result
                   (list (match-start byte-crlf-result)
                         (match-end byte-crlf-result)))
              :to-equal
              '(1 1)))))
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
  (let* ((name (string (code-char #x00e9)))
         (g-pattern (format nil "(?<~A>a)\\g{~A}" name name))
         (k-pattern (format nil "(?<~A>a)\\k<~A>" name name))
         (subroutine-pattern (format nil "(?<~A>a)(?&~A)" name name))
         (conditional-pattern (format nil "(?<~A>a)(?(~A)b|c)" name name))
         (g-brace (compile-regex g-pattern))
         (k-angle (compile-regex k-pattern))
         (subroutine (compile-regex subroutine-pattern))
         (conditional (compile-regex conditional-pattern))
         (g-result (scan g-brace "aa"))
         (k-result (scan k-angle "aa"))
         (subroutine-result (scan subroutine "aa"))
         (conditional-result (scan conditional "ab")))
    (dolist (regex (list g-brace k-angle subroutine conditional))
      (expect (regex-advanced-p regex) :to-be-truthy))
    (dolist (result (list g-result k-result subroutine-result conditional-result))
      (expect (and result
                   (list (match-start result)
                         (match-end result)))
              :to-equal
              (list 0 2)))))
(it
  "rejects unresolved advanced references during compilation"
  (dolist (pattern
           '("(?P=missing)"
             "(?&missing)"
             "(?P>missing)"
             "(?(missing)a|b)"
             "(?(R&missing)a|b)"
             "(?1)"))
    (signals regex-syntax-error (compile-regex pattern))))
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
                   (format nil "(?C~Ctag~C)a" (code-char 39) (code-char 39))
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
(it "supports Unicode grapheme, word, and sentence boundary anchors"
  (let* ((grapheme-text (format nil "a~C" (code-char #x301)))
         (grapheme (compile-regex "\\b{g}\\X\\b{g}"))
         (word (compile-regex "\\b{wb}word\\b{wb}"))
         (sentence (compile-regex "\\b{sb}B"))
         (sentence-lower (compile-regex "\\b{sb}b"))
         (crlf (compile-regex "\\r\\b{sb}\\n"))
         (grapheme-result (scan grapheme grapheme-text))
         (word-result (scan word "word"))
         (sentence-result (scan sentence "A. B"))
         (sentence-lower-result (scan sentence-lower "A. b"))
         (crlf-result
           (scan crlf (format nil "A~C~CB" #\Return #\Linefeed)))
         (byte-grapheme-text
           (make-array 3
                       :element-type '(unsigned-byte 8)
                       :initial-contents '(97 204 129)))
         (byte-grapheme
           (compile-byte-regex "(?u:\\b{g}\\X\\b{g})"))
         (byte-word-text
           (make-array 4
                       :element-type '(unsigned-byte 8)
                       :initial-contents '(97 195 169 33)))
         (byte-word
           (compile-byte-regex "(?u:\\b{wb}\\p{L}+\\b{wb})"))
         (byte-sentence-text
           (make-array 4
                       :element-type '(unsigned-byte 8)
                       :initial-contents '(65 46 32 66)))
         (byte-sentence
           (compile-byte-regex "(?u:\\b{sb}B)"))
         (invalid-byte-text
           (make-array 2
                       :element-type '(unsigned-byte 8)
                       :initial-contents '(255 65)))
         (invalid-byte-sentence
           (compile-byte-regex "(?u:\\b{sb}A)"))
         (byte-grapheme-result
           (scan byte-grapheme byte-grapheme-text))
         (byte-word-result
           (scan byte-word byte-word-text))
         (byte-sentence-result
           (scan byte-sentence byte-sentence-text))
         (invalid-byte-result
           (scan invalid-byte-sentence invalid-byte-text)))
    (dolist (regex
             (list grapheme word sentence sentence-lower crlf
                   byte-grapheme byte-word byte-sentence
                   invalid-byte-sentence))
      (expect (regex-advanced-p regex) :to-be-truthy))
    (expect (and grapheme-result
                 (list (match-start grapheme-result)
                       (match-end grapheme-result)))
            :to-equal
            '(0 2))
    (expect (and word-result
                 (list (match-start word-result)
                       (match-end word-result)))
            :to-equal
            '(0 4))
    (expect (and sentence-result
                 (list (match-start sentence-result)
                       (match-end sentence-result)))
            :to-equal
            '(3 4))
    (expect sentence-lower-result :to-be-null)
    (expect crlf-result :to-be-null)
    (expect (and byte-grapheme-result
                 (list (match-start byte-grapheme-result)
                       (match-end byte-grapheme-result)))
            :to-equal
            '(0 3))
    (expect (and byte-word-result
                 (list (match-start byte-word-result)
                       (match-end byte-word-result)))
            :to-equal
            '(0 3))
    (expect (and byte-sentence-result
                 (list (match-start byte-sentence-result)
                       (match-end byte-sentence-result)))
            :to-equal
            '(3 4))
    (expect (and invalid-byte-result
                 (list (match-start invalid-byte-result)
                       (match-end invalid-byte-result)))
            :to-equal
            '(1 2))))

(it "evaluates grapheme boundaries at interior positions"
  (let* ((text (format nil "a~Cb" (code-char #x301)))
         (regex (compile-regex "\\X\\b{g}b"))
         (result (scan regex text)))
    (expect result :to-be-truthy)
    (expect (and result
                 (list (match-start result)
                       (match-end result)))
            :to-equal
            '(0 3))))
