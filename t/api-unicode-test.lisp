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
    (expect join-control :to-match-regex (compile-regex "\\w"))
    (expect-not join-control :to-match-regex (compile-regex "(?-u:\\w)")))
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
  (expect "é" :to-match-regex (compile-regex "(?-u:é)"))
  (expect "word_42" :to-match-regex (compile-regex "(?-u:\\w+)"))
  (expect "a" :to-match-regex (compile-regex "(?-u:[a])"))
  (dolist (pattern '("(?-u:\\xE9)"
                     "(?-u:\\351)"
                     "(?-u:[é])"
                     "(?-u:[^a])"
                     "(?-u:[\\p{L}])"
                     "(?-u:\\D)"
                     "(?-u:.)"))
    (signals regex-syntax-error (compile-regex pattern)))
  (expect (make-array 1 :element-type '(unsigned-byte 8) :initial-contents '(233))
          :to-match-regex (compile-byte-regex "(?-u:\\xE9)")))

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
    (expect (octets #xc3 #xa9) :to-match-regex (compile-byte-regex "(?-u:é)"))
    (expect-not (octets #xe9) :to-match-regex (compile-byte-regex "(?-u:é)"))
    (expect (octets #xe2 #x98 #x83) :to-match-regex (compile-byte-regex "(?-u:☃)"))
    (expect (octets #xc3 #xa9) :to-match-regex (compile-byte-regex "(?-u:\\x{E9})"))
    (expect (octets #xc3 #xa9) :to-match-regex (compile-byte-regex "(?-u:\\u00E9)"))
    (expect (octets #xe2 #x98 #x83)
            :to-match-regex (compile-byte-regex "(?-u:\\U{2603})"))
    (expect-not (octets #xc3 #x89) :to-match-regex (compile-byte-regex "(?i-u:é)"))
    (expect (octets #xe9) :to-match-regex (compile-byte-regex "(?-u:\\xE9)"))
    (expect (octets #xe9) :to-match-regex (compile-byte-regex "(?-u:[\\xE9])"))
    (expect (octets #x61) :to-match-regex (compile-byte-regex "(?-u:[\\u0061])"))
    (expect (octets #x7f) :to-match-regex (compile-byte-regex "(?-u:[\\x{7F}])"))
    (signals regex-syntax-error (compile-byte-regex "(?-u:[é])"))
    (signals regex-syntax-error (compile-byte-regex "(?-u:[☃])"))
      (signals regex-syntax-error (compile-byte-regex "(?-u:[\\x{E9}])"))
      (signals regex-syntax-error (compile-byte-regex "(?-u:[\\u{E9}])"))))

(it "decodes valid UTF-8 scalars and rejects malformed byte sequences"
  (flet ((octets (&rest values)
           (make-array (length values) :element-type '(unsigned-byte 8)
                       :initial-contents values))
         (decode-at (text position)
           (multiple-value-list (cl-regex-kit::utf8-character-at text position))))
    (dolist (case `(((65) 0 (#\A 1 t))
                    ((195 169) 0 (,(code-char #x00e9) 2 t))
                    ((226 152 131) 0 (,(code-char #x2603) 3 t))
                    ((240 159 152 128) 0 (,(code-char #x1f600) 4 t))))
      (destructuring-bind (values position expected) case
        (expect (decode-at (apply #'octets values) position) :to-equal expected)))
    (dolist (values '((192 128)
                      (128)
                      (194)
                      (195 40)
                      (224 159 128)
                      (226 130)
                      (226 40 129)
                      (237 160 128)
                      (240 143 128 128)
                      (240 159 152)
                      (240 159 40 128)
                      (240 159 152 40)
                      (244 144 128 128)
                      (245 128 128 128)))
      (expect (decode-at (apply #'octets values) 0) :to-equal '(nil)))
    (expect (decode-at (octets #x61) 1) :to-equal '(nil))
    (expect (multiple-value-list
             (cl-regex-kit::utf8-character-before (octets 195 169) 2))
            :to-equal (list (code-char #x00e9) 0 t))
    (let ((text (octets #x61 #xc3 #xa9)))
      (expect (cl-regex-kit::byte-unicode-non-boundary-position-p text 1)
              :to-be-truthy)
      (expect (cl-regex-kit::byte-unicode-non-boundary-position-p text 2)
              :to-be-null)
      (expect (cl-regex-kit::byte-unicode-non-boundary-position-p text 3)
              :to-be-truthy))))

(it "decodes UTF-8 scalars backward without accepting malformed boundaries"
  (flet ((octets (&rest values)
           (make-array (length values)
                       :element-type (quote (unsigned-byte 8))
                       :initial-contents values))
         (decode-before (text position)
           (multiple-value-list
            (cl-regex-kit::utf8-character-before text position))))
    (dolist (case
             (list
              (list (list 65) 1 (list #\A 0 t))
              (list (list 195 169) 2 (list (code-char #x00e9) 0 t))
              (list (list 226 152 131) 3 (list (code-char #x2603) 0 t))
              (list (list 240 159 152 128) 4
                    (list (code-char #x1f600) 0 t))))
      (destructuring-bind (values position expected) case
        (expect (decode-before (apply (function octets) values) position)
                :to-equal expected)))
    (dolist (case
             (list
              (list (list 128) 1)
              (list (list 194) 1)
              (list (list 195 169) 1)
              (list (list 97 128) 2)
              (list (list 226 130) 2)
              (list (list 240 159 152) 3)))
      (destructuring-bind (values position) case
        (expect (decode-before (apply (function octets) values) position)
                :to-equal (list nil nil nil))))
    (let ((text (octets #x61 #xc3 #xa9)))
      (expect (decode-before text 0) :to-equal (list nil nil nil))
      (expect (decode-before text 4) :to-equal (list nil nil nil)))))


(it "evaluates composed character and byte class matchers"
  (let ((ranges '(:ranges ((97 . 99))))
        (property (list :property (cl-regex-kit::resolve-unicode-property "Lu"))))
    (expect (cl-regex-kit::matcher-matches-p ranges #\b) :to-be-truthy)
    (expect (cl-regex-kit::matcher-matches-p property #\A) :to-be-truthy)
    (expect (cl-regex-kit::matcher-matches-p `(:union ,ranges ,property) #\A)
            :to-be-truthy)
    (expect (cl-regex-kit::matcher-matches-p `(:intersection ,ranges ,ranges) #\b)
            :to-be-truthy)
    (expect (cl-regex-kit::matcher-matches-p `(:difference ,ranges (:ranges ((98 . 98)))) #\a)
            :to-be-truthy)
    (expect (cl-regex-kit::matcher-matches-p `(:symmetric-difference ,ranges (:ranges ((98 . 100)))) #\a)
            :to-be-truthy)
    (expect (cl-regex-kit::matcher-matches-p `(:negate ,ranges) #\z)
            :to-be-truthy)
    (signals error (cl-regex-kit::matcher-matches-p '(:unknown) #\a)))
  (let ((ranges '(:ranges ((#\a . #\c)))))
    (dolist (case `((,ranges 98 t)
                    ((:union ,ranges) 98 t)
                    ((:intersection ,ranges (:ranges ((98 . 99)))) 98 t)
                    ((:difference ,ranges (:ranges ((98 . 98)))) 97 t)
                    ((:symmetric-difference ,ranges (:ranges ((98 . 100)))) 97 t)
                    ((:negate ,ranges) 122 t)
                    ((:unknown) 97 nil)))
      (destructuring-bind (matcher octet expected) case
        (expect (not (null (cl-regex-kit::byte-matcher-matches-p matcher octet)))
                :to-equal expected)))))

(it "handles byte-class folding, boundaries, and CRLF positions"
  (flet ((octets (&rest values)
           (make-array (length values) :element-type '(unsigned-byte 8)
                       :initial-contents values)))
    (let ((unicode-class
            (make-instance 'cl-regex-kit::char-class-node
                           :ranges nil
                           :case-insensitive-p t
                           :unicode-p t))
          (byte-class
            (make-instance 'cl-regex-kit::char-class-node
                           :ranges '((#\a #\c))
                           :case-insensitive-p t
                           :unicode-p nil)))
      (expect (cl-regex-kit::class-matches-p unicode-class #\A) :to-be-null)
      (expect (cl-regex-kit::class-matches-octet-p byte-class #x42)
              :to-be-truthy)
      (expect (cl-regex-kit::class-matches-octet-p byte-class #x63)
              :to-be-truthy))
    (expect (cl-regex-kit::byte-matcher-matches-p
             '(:symmetric-difference (:ranges ((97 . 99))) (:ranges ((98 . 100))))
             #x62)
            :to-be-null)
    (let ((text (octets #x61 #xc3 #xa9 #x21)))
      (expect (cl-regex-kit::byte-word-boundary-p text 1) :to-be-truthy)
      (expect (cl-regex-kit::byte-word-start-p text 1) :to-be-null)
      (expect (cl-regex-kit::byte-word-end-p text 1) :to-be-truthy)
      (expect (cl-regex-kit::byte-unicode-word-boundary-p text 1) :to-be-null)
      (expect (cl-regex-kit::byte-unicode-word-boundary-p text 2) :to-be-null))
    (let ((crlf (octets #x61 #x0d #x0a #x62)))
      (expect (cl-regex-kit::byte-line-start-p crlf 2 t #\Newline) :to-be-null)
      (expect (cl-regex-kit::byte-line-start-p crlf 3 t #\Newline) :to-be-truthy)
      (expect (cl-regex-kit::byte-line-end-p crlf 1 t #\Newline) :to-be-truthy)
      (expect (cl-regex-kit::byte-line-end-p crlf 2 t #\Newline) :to-be-null))))

(it "distinguishes byte and Unicode boundary edge cases"
  (flet ((octets (&rest values)
           (make-array (length values) :element-type '(unsigned-byte 8)
                       :initial-contents values)))
    (expect (cl-regex-kit::ascii-case-insensitive-char= #\A #\a) :to-be-truthy)
    (expect (cl-regex-kit::ascii-case-insensitive-char=
             (code-char #x00e9) (code-char #x00c9))
            :to-be-null)
    (expect (cl-regex-kit::byte-matcher-matches-p
             '(:symmetric-difference (:ranges ((97 . 97)))
                                    (:ranges ((98 . 98))))
             #x62)
            :to-be-truthy)
    (let ((text (octets #x61 #x21)))
      (expect (cl-regex-kit::byte-unicode-word-start-p text 0) :to-be-truthy)
      (expect (cl-regex-kit::byte-unicode-word-end-p text 1) :to-be-truthy)
      (expect (cl-regex-kit::byte-unicode-word-start-half-p text 0) :to-be-truthy)
      (expect (cl-regex-kit::byte-unicode-word-end-half-p text 1) :to-be-truthy))
    (let ((text (format nil "a~C~Cb" #\Return #\Newline)))
      (expect (cl-regex-kit::line-start-p text 2 t #\Newline) :to-be-null)
      (expect (cl-regex-kit::line-start-p text 3 t #\Newline) :to-be-truthy)
      (expect (cl-regex-kit::line-end-p text 1 t #\Newline) :to-be-truthy)
      (expect (cl-regex-kit::line-end-p text 2 t #\Newline) :to-be-null))))

(it "keeps string and UTF-8 octet boundary domains distinct at input edges"
  (flet ((octets (&rest values)
           (make-array (length values)
                       :element-type (quote (unsigned-byte 8))
                       :initial-contents values)))
    (let ((text "a"))
      (expect (cl-regex-kit::word-start-p text 0 t) :to-be-truthy)
      (expect (cl-regex-kit::word-end-p text 1 t) :to-be-truthy)
      (expect (cl-regex-kit::word-boundary-p text 0 t) :to-be-truthy)
      (expect (cl-regex-kit::word-boundary-p text 1 t) :to-be-truthy))
    (let ((text (octets #xc3 #xa9)))
      (expect (cl-regex-kit::byte-unicode-word-start-p text 0) :to-be-truthy)
      (expect (cl-regex-kit::byte-unicode-word-end-p text 2) :to-be-truthy)
      (expect (cl-regex-kit::byte-unicode-word-boundary-p text 0) :to-be-truthy)
      (expect (cl-regex-kit::byte-unicode-word-boundary-p text 1) :to-be-null)
      (expect (cl-regex-kit::byte-unicode-word-boundary-p text 2) :to-be-truthy))
    (let ((malformed (octets #x80)))
      (expect (cl-regex-kit::byte-unicode-non-boundary-position-p malformed 0)
              :to-be-null)
      (expect (cl-regex-kit::byte-unicode-non-boundary-position-p malformed 1)
              :to-be-null))))
(it
  "executes pre-resolved Unicode descriptors without resolver work"
  (let ((explicit (compile-regex "\\p{Lu}+"))
        (negated (compile-regex "\\P{Lu}+"))
        (digit (compile-regex "\\d+"))
        (word (compile-regex "\\w+"))
        (space (compile-regex "\\s+"))
        (class-property (compile-regex "[\\p{Greek}]+")))
    (expect "ABC" :to-match-regex explicit)
    (expect "abc" :to-match-regex negated)
    (expect (string (code-char #xff11)) :to-match-regex digit)
    (expect (string (code-char #x200c)) :to-match-regex word)
    (expect (string (code-char #x3000)) :to-match-regex space)
    (expect "α" :to-match-regex class-property))
  (dolist (case `(("Lu" #\A t)
                  ("Lu" #\a nil)
                  ("Nd" ,(code-char #xff11) t)
                  ("White_Space" ,(code-char #x3000) t)
                  ("Greek" #\α t)))
    (destructuring-bind (name character expected) case
      (let ((descriptor
              (or (cl-regex-kit::resolve-unicode-property name)
                  (error "Unicode property did not resolve: ~A" name))))
        (expect
          (not
            (null
              (cl-regex-kit::unicode-property-descriptor-matches-p
                descriptor
                character)))
          :to-equal
          expected)))))

(it
  "resolves properties that fall outside SBCL's own grapheme-break classification"
  (expect (string (code-char #x09be)) :to-match-regex (compile-regex "\\p{Grapheme_Extend}")))

(it
  "matches noncharacter code points via the fast >= #xfffe range"
  (expect-not "a" :to-match-regex (compile-regex "\\p{NChar}"))
  (expect (string (code-char #xfffe)) :to-match-regex (compile-regex "\\p{NChar}"))
  (expect-not (string (code-char #xfffd)) :to-match-regex (compile-regex "\\p{NChar}")))

(it
  "matches packed Unicode ranges at binary-search boundaries"
  (labels ((matches-p (packed code)
             (cl-regex-kit::packed-unicode-ranges-match-code-p packed code)))
    (let ((packed (cl-regex-kit::pack-unicode-property-ranges nil)))
      (expect (length packed) :to-equal 0)
      (expect (matches-p packed 0) :to-be-null)
      (expect (matches-p packed (1- char-code-limit)) :to-be-null))
    (let ((packed
            (cl-regex-kit::pack-unicode-property-ranges
              (loop for lower from 0 below 24 by 3
                    collect (cons lower (1+ lower))))))
      (expect (matches-p packed 0) :to-be-truthy)
      (expect (matches-p packed 22) :to-be-truthy)
      (expect (matches-p packed 2) :to-be-null)
      (expect (matches-p packed 23) :to-be-null)
      (expect (matches-p packed (1- char-code-limit)) :to-be-null))
    (let ((packed
            (cl-regex-kit::pack-unicode-property-ranges
              `((0 . 1)
                (3 . 4)
                (6 . 7)
                (9 . 10)
                (12 . 13)
                (15 . 16)
                (18 . 19)
                (21 . 22)
                (,(1- char-code-limit) . ,(1- char-code-limit))))))
      (expect (matches-p packed 0) :to-be-truthy)
      (expect (matches-p packed 22) :to-be-truthy)
      (expect (matches-p packed 2) :to-be-null)
      (expect (matches-p packed 23) :to-be-null)
      (expect (matches-p packed (1- char-code-limit)) :to-be-truthy))))
(it
  "rejects malformed packed Unicode ranges"
  (signals error
    (cl-regex-kit::pack-unicode-property-ranges `((0 . ,char-code-limit))))
  (signals error
    (cl-regex-kit::pack-unicode-property-ranges `((,(1- char-code-limit) . ,char-code-limit))))
  (signals error
    (cl-regex-kit::pack-unicode-property-ranges (quote ((-1 . 0)))))
  (signals error
    (cl-regex-kit::pack-unicode-property-ranges (quote ((3 . 2)))))
  (signals error
    (cl-regex-kit::pack-unicode-property-ranges (quote ((3 . 4) (1 . 2)))))
  (signals error
    (cl-regex-kit::pack-unicode-property-ranges (quote ((1 . 3) (3 . 4))))))
(it
  "treats representative Unicode property aliases equivalently"
  (labels ((matches-p (property character)
             (let ((descriptor
                     (or (cl-regex-kit::resolve-unicode-property property)
                         (error "Unicode property did not resolve: ~A" property))))
               (not
                 (null
                   (cl-regex-kit::unicode-property-descriptor-matches-p
                     descriptor
                     character)))))
           (expect-equivalent (left right character)
             (expect (matches-p left character)
                     :to-equal
                     (matches-p right character))))
    (expect-equivalent "Lu" "General_Category=Uppercase_Letter" #\A)
    (expect-equivalent "sc=Grek" "Script=Greek" #\α)
    (expect-equivalent "scx=Grek" "Script_Extensions=Greek" #\α)
    (expect-equivalent "blk=Basic_Latin" "InBasic_Latin" #\A)
    (expect-equivalent "age=1.1" "Age=V1_1" #\A)
    (expect-equivalent "GCB=EX" "Grapheme_Cluster_Break=Extend"
                       (code-char #x0301))
    (expect-equivalent "WB=LE" "Word_Break=ALetter" #\a)
    (expect-equivalent "SB=UP" "Sentence_Break=Upper" #\A)))
(it "uses finite SBCL Unicode metadata without keyword-package coupling"
  (labels ((descriptor (name)
             (or (cl-regex-kit::resolve-unicode-property name)
                 (error "Unicode property did not resolve: ~A" name)))
           (matches-p (name character)
             (cl-regex-kit::unicode-property-descriptor-matches-p
              (descriptor name)
              character)))
    (dolist (case (list
                   (list "General_Category=Uppercase_Letter" #\A)
                   (list "Script=Greek" #\α)
                   (list "Script_Extensions=Greek" #\α)
                   (list "Block=Basic_Latin" #\A)
                   (list "Grapheme_Cluster_Break=Extend" (code-char #x0301))
                   (list "Word_Break=ALetter" #\a)
                   (list "Sentence_Break=Upper" #\A)))
      (expect (matches-p (first case) (second case)) :to-be-truthy))
    (dolist (case (quote ((:category "LU" "Lu")
                          (:script "GREEK" "Script=Greek")
                          (:block "BASICLATIN" "Block=Basic_Latin")
                          (:grapheme-break "EXTEND" "GCB=Extend")
                          (:word-break "ALETTER" "WB=ALetter")
                          (:sentence-break "UPPER" "SB=Upper"))))
      (destructuring-bind (domain value-name property-name) case
        (multiple-value-bind (value present-p)
            (cl-regex-kit::unicode-runtime-property-value domain value-name)
          (expect present-p :to-be-truthy)
          (expect
           (eq value
               (cl-regex-kit::unicode-property-descriptor-payload
                (descriptor property-name)))
           :to-be-truthy))))
    (dolist (case (quote ((:grapheme-break "GCB=Other")
                          (:word-break "WB=Other")
                          (:sentence-break "SB=Other"))))
      (destructuring-bind (domain property-name) case
        (multiple-value-bind (value present-p)
            (cl-regex-kit::unicode-runtime-property-value domain "OTHER")
          (expect value :to-be-null)
          (expect present-p :to-be-truthy)
          (expect
           (cl-regex-kit::unicode-property-descriptor-payload
            (descriptor property-name))
           :to-be-null))))
    (let ((payload-before-keyword-intern
            (cl-regex-kit::unicode-property-descriptor-payload
             (descriptor "Script=Greek"))))
      (intern "G_R_E_E_K" "KEYWORD")
      (expect
       (eq payload-before-keyword-intern
           (cl-regex-kit::unicode-property-descriptor-payload
            (descriptor "Script=Greek")))
       :to-be-truthy))
    (signals error
      (cl-regex-kit::make-unicode-runtime-domain-index
       :script
       (list (make-symbol "A-B") (make-symbol "A_B"))))
    (let* ((middle-dot (code-char #x00b7))
           (script
             (cl-regex-kit::unicode-property-descriptor-payload
              (descriptor "Script=Greek")))
           (script-extension
             (cl-regex-kit::unicode-property-descriptor-payload
              (descriptor "Script_Extensions=Greek"))))
      (expect (eq (sb-unicode:script middle-dot) script) :to-be-null)
      (expect (eq (car script-extension) script) :to-be-truthy)
      (expect (matches-p "Script_Extensions=Greek" #\A) :to-be-null)
      (expect (matches-p "Script_Extensions=Greek" middle-dot)
              :to-be-truthy))))
(it "rejects unknown Unicode descriptor and metadata inputs"
  (signals error
    (cl-regex-kit::unicode-property-descriptor-matches-p
      (cl-regex-kit::%make-unicode-property-descriptor :unknown nil)
      #\A))
  (signals error
    (cl-regex-kit::unicode-runtime-domain-index :unknown))
  (signals error
    (cl-regex-kit::unicode-runtime-property-value :unknown "VALUE"))
  (signals error
    (cl-regex-kit::unicode-runtime-category-values
      (list "UNKNOWNCATEGORY")))
  (dolist (property (list "GCB=UnknownValue"
                          "WB=UnknownValue"
                          "SB=UnknownValue"))
    (expect (cl-regex-kit::resolve-unicode-property property)
            :to-be-null)))
(it "classifies ASCII and Unicode word boundaries by authoritative categories"
  (expect (cl-regex-kit::word-character-p #\_ nil) :to-be-truthy)
  (expect (cl-regex-kit::word-character-p #\! nil) :to-be-null)
  (expect (cl-regex-kit::word-boundary-p "a_!" 1 nil) :to-be-null)
  (expect (cl-regex-kit::word-boundary-p "a_!" 2 nil) :to-be-truthy)
  (dolist (code (quote (#x0301 #x0903 #x0488 #x0660 #x203f #x200c #x200d)))
    (let* ((character (code-char code))
           (text (format nil "a~C!" character)))
      (expect (cl-regex-kit::word-character-p character t) :to-be-truthy)
      (expect (cl-regex-kit::word-boundary-p text 1 t) :to-be-null)
      (expect (cl-regex-kit::word-boundary-p text 2 t) :to-be-truthy)))
  (let ((symbol (code-char #x2603)))
    (expect (cl-regex-kit::word-character-p symbol t) :to-be-null)
    (expect (cl-regex-kit::word-boundary-p (format nil "a~C" symbol) 1 t)
            :to-be-truthy)))
