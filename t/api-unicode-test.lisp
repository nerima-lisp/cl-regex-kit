(in-package #:cl-regex-kit/test)

(it
  "supports absolute anchors, ASCII word boundaries, and named captures"
  (expect (match "\\Aabc\\z" "xabc") :to-be-null)
  (expect (match-string (match "\\Aabc\\z" "abc") "abc") :to-equal "abc")
  (expect (match-string (match "\\bcat\\b" "a cat!") "a cat!") :to-equal "cat")
  (expect
    (match-string (match "\\Bcat\\B" "scatterplot") "scatterplot")
    :to-equal
    "cat")
  (expect
    (match-string (match "\\b{start}\\w+\\b{end}" "!!hello??") "!!hello??")
    :to-equal
    "hello")
  (expect (match-string (match "\\b{start-half}a" "!!a") "!!a") :to-equal "a")
  (expect (match-string (match "a\\b{end-half}" "a!!") "a!!") :to-equal "a")
  (expect
    (match-string (match "\\<\\w+\\>" "!!hello??") "!!hello??")
    :to-equal
    "hello")
  (let* ((regex (compile-regex "(?<word>[a-z]+)-(?P<number>\\d+)"))
         (result (scan regex "item-42")))
    (expect (regex-group-count regex) :to-equal 2)
    (expect (regex-capture-count regex) :to-equal 3)
    (expect (regex-static-capture-count regex) :to-equal 3)
    (signals type-error (regex-capture-count nil))
    (expect (regex-group-index regex "word") :to-equal 1)
    (expect (regex-group-index regex "number") :to-equal 2)
    (let ((names (regex-capture-names regex)))
      (unless (equalp names #(nil "word" "number"))
        (error "REGEX-CAPTURE-NAMES did not preserve capture indices"))
      (setf (aref names 1) "changed")
      (unless (equalp (regex-capture-names regex) #(nil "word" "number"))
        (error "REGEX-CAPTURE-NAMES exposed mutable regex state")))
    (unless (equalp (match-captures result "item-42")
                    #("item-42" "item" "42"))
      (error "MATCH-CAPTURES did not preserve capture order"))
    (expect (match-group-string result "word" "item-42") :to-equal "item")
    (expect (match-group-string result "number" "item-42") :to-equal "42"))
  (let ((result (scan (compile-regex "(a)(b)?") "a")))
    (unless (equalp (match-captures result "a") #("a" "a" nil))
      (error "MATCH-CAPTURES did not preserve a missing optional capture")))
  (dolist (case '(("a" 1)
                  ("(a)|(b)" 2)
                  ("(a)(b)|(c)(d)" 3)
                  ("(b)+" 2)
                  ("(?:a?)*" 1)
                  ("(a)|b" nil)
                  ("a|(b)" nil)
                  ("(b)*" nil)
                  ("((a)|b)" nil)))
    (destructuring-bind (pattern expected) case
      (expect (regex-static-capture-count (compile-regex pattern)) :to-equal expected)))
  (expect (regex-static-capture-count (compile-byte-regex "(a)|(b)"))
          :to-equal 2)
  (signals type-error (regex-capture-names nil))
  (let* ((regex (compile-regex "(?<part_name_2>abc)"))
         (result (scan regex "abc")))
    (expect (regex-group-index regex "part_name_2") :to-equal 1)
    (expect (match-group-string result "part_name_2" "abc") :to-equal "abc")
    (expect (replace-first regex "abc" "$part_name_2") :to-equal "abc"))
  (let* ((regex (compile-regex "(?<Δ.part[2]>abc)"))
         (result (scan regex "abc")))
    (expect (regex-group-index regex "Δ.part[2]") :to-equal 1)
    (expect (match-group-string result "Δ.part[2]" "abc") :to-equal "abc")
    (expect (replace-first regex "abc" "${Δ.part[2]}") :to-equal "abc"))
  (signals regex-syntax-error
    (compile-regex (format nil "(?<a~Cb>abc)" (code-char #x203f)))))

(it
  "supports Unicode shorthands, properties, and word boundaries by default"
  (let ((join-control (string (code-char #x200c))))
    (expect (is-match-p (compile-regex "\\w") join-control) :to-be-truthy)
    (expect (is-match-p (compile-regex "(?-u:\\w)") join-control) :to-be-null))
  (let ((text (format nil "~Cclair!" (code-char #x00e9))))
    (expect (match-string (match "\\w+" text) text) :to-equal (subseq text 0 6))
    (expect
      (match-string (match "\\b\\w+\\b" text) text)
      :to-equal
      (subseq text 0 6)))
  (expect
    (match-string
      (match "\\d+" (string (code-char #xff11)))
      (string (code-char #xff11)))
    :to-equal
    (string (code-char #xff11)))
  (expect (match-string (match "\\p{Lu}+" "--ABcd") "--ABcd") :to-equal "AB")
  (expect (match-string (match "\\p{L}+" "--AB12") "--AB12") :to-equal "AB")
  (expect (match-string (match "\\pL+" "--AB12") "--AB12") :to-equal "AB")
  (expect (match-string (match "\\PL+" "AB12") "AB12") :to-equal "12")
  (expect (match-string (match "\\p{LC}+" "--Ab1") "--Ab1") :to-equal "Ab")
  (expect
    (match-string
      (match "\\p{General_Category=Uppercase_Letter}+" "--ABcd")
      "--ABcd")
    :to-equal
    "AB")
  (expect (match-string (match "\\p{scx:Greek}+" "--αβ--") "--αβ--")
          :to-equal "αβ")
  (expect (match-string (match "\\p{gc!=Lu}+" "--ABcd") "--ABcd")
          :to-equal "--")
  (expect (match-string (match "\\P{gc!=Lu}+" "--ABcd") "--ABcd")
          :to-equal "AB")
  (expect (match-string (match "[\\D]+" "12ab34") "12ab34") :to-equal "ab"))

(it
  "scopes Unicode shorthands and word boundaries"
  (let ((text (format nil "~Cclair" (code-char #x00e9))))
    (expect (match-string (match "\\w+" text) text) :to-equal text)
    (expect (match-string (match "(?-u:\\w+)" text) text) :to-equal "clair")
    (expect (match-string (match "\\b\\w+\\b" text) text) :to-equal text)
    (expect (match-string (match "(?-u:\\b\\w+\\b)" text) text) :to-equal "clair"))
  (let ((vertical-tab (string (code-char 11))))
    (expect
      (match-string
        (scan (compile-regex "\\s" :unicode nil) vertical-tab)
        vertical-tab)
      :to-equal vertical-tab)
    (signals regex-syntax-error (compile-regex "\\S" :unicode nil))))

(it
  "preserves Rust Regex UTF-8 invariants outside the bytes API"
  (expect (is-match-p (compile-regex "(?-u:é)") "é") :to-be-truthy)
  (expect (is-match-p (compile-regex "(?-u:\\w+)") "word_42") :to-be-truthy)
  (dolist (pattern '("(?-u:\\xE9)"
                     "(?-u:\\351)"
                     "(?-u:[é])"
                     "(?-u:[^a])"
                     "(?-u:\\D)"
                     "(?-u:.)"))
    (signals regex-syntax-error (compile-regex pattern)))
  (expect (is-match-p (compile-byte-regex "(?-u:\\xE9)")
                      (make-array 1 :element-type '(unsigned-byte 8)
                                   :initial-contents '(233)))
          :to-be-truthy))

(it
  "does not let Unicode non-boundaries split UTF-8 byte sequences"
  (let ((text (make-array 2 :element-type '(unsigned-byte 8)
                          :initial-contents '(#xc3 #xa9))))
    (expect (scan (compile-byte-regex "\\B(?-u:\\xA9)") text) :to-be-null)
    (expect (match-start (scan (compile-byte-regex "(?-u:\\B\\xA9)") text))
            :to-equal
            1)))

(it
  "encodes raw byte-scope source literals as UTF-8"
  (flet ((octets (&rest values)
           (make-array (length values) :element-type '(unsigned-byte 8)
                       :initial-contents values)))
    (expect (is-match-p (compile-byte-regex "(?-u:é)") (octets #xc3 #xa9))
            :to-be-truthy)
    (expect (is-match-p (compile-byte-regex "(?-u:é)") (octets #xe9))
            :to-be-null)
    (expect (is-match-p (compile-byte-regex "(?-u:☃)") (octets #xe2 #x98 #x83))
            :to-be-truthy)
    (expect (is-match-p (compile-byte-regex "(?-u:\\x{E9})") (octets #xc3 #xa9))
            :to-be-truthy)
    (expect (is-match-p (compile-byte-regex "(?-u:\\u00E9)") (octets #xc3 #xa9))
            :to-be-truthy)
    (expect (is-match-p (compile-byte-regex "(?-u:\\U{2603})")
                        (octets #xe2 #x98 #x83))
            :to-be-truthy)
    (expect (is-match-p (compile-byte-regex "(?i-u:é)") (octets #xc3 #x89))
            :to-be-null)
    (expect (is-match-p (compile-byte-regex "(?-u:\\xE9)") (octets #xe9))
            :to-be-truthy)
    (expect (is-match-p (compile-byte-regex "(?-u:[\\xE9])") (octets #xe9))
            :to-be-truthy)
    (expect (is-match-p (compile-byte-regex "(?-u:[\\u0061])") (octets #x61))
            :to-be-truthy)
    (expect (is-match-p (compile-byte-regex "(?-u:[\\x{7F}])") (octets #x7f))
            :to-be-truthy)
    (signals regex-syntax-error (compile-byte-regex "(?-u:[é])"))
    (signals regex-syntax-error (compile-byte-regex "(?-u:[☃])"))
    (signals regex-syntax-error (compile-byte-regex "(?-u:[\\x{E9}])"))
    (signals regex-syntax-error (compile-byte-regex "(?-u:[\\u{E9}])"))))
