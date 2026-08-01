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
    (expect (is-match-p regex text) :to-be-truthy)
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
    (scan-captures-into regex locations "123")
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
