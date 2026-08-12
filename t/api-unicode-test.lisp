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

(it-each (("a" 1)
          ("(a)|(b)" 2)
          ("(a)(b)|(c)(d)" 3)
          ("(b)+" 2)
          ("(?:a?)*" 1)
          ("(a)|b" nil)
          ("a|(b)" nil)
          ("(b)*" nil)
          ("((a)|b)" nil))
    "computes static capture count for ~S"
    (pattern expected)
  (expect (regex-static-capture-count (compile-regex pattern)) :to-equal expected))

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
  (expect (make-array 1 :element-type '(unsigned-byte 8) :initial-contents '(233))
          :to-match-regex (compile-byte-regex "(?-u:\\xE9)")))

(it-each (("(?-u:\\xE9)")
          ("(?-u:\\351)")
          ("(?-u:[é])")
          ("(?-u:[^a])")
          ("(?-u:[\\p{L}])")
          ("(?-u:\\D)")
          ("(?-u:.)"))
    "rejects Unicode-unsafe byte-scope pattern ~S outside the bytes API"
    (pattern)
  (signals regex-syntax-error (compile-regex pattern)))

(it
  "rejects non-scalar Common Lisp characters at the Unicode boundary"
  (let ((surrogate (code-char #xd800)))
    (when surrogate
      (let ((text (string surrogate)))
        (signals regex-syntax-error (compile-regex text))
        (signals regex-syntax-error (compile-byte-regex text))
        (signals type-error (scan (compile-regex ".") text))
        (expect (scan (compile-regex ".") text :start 0 :end 0)
                :to-be-null)
        (expect (match-start
                  (scan (compile-regex ".")
                        (concatenate 'string text "a")
                        :start 1))
                :to-equal 1)))))

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
  (signals regex-syntax-error (compile-byte-regex "(?-u:[\\u{E9}])")))

(it
 "decodes valid UTF-8 scalars consistently with cl-codec-kit fixtures"
 (dolist (text '("A" "é" "☃" "😀"))
   (let ((octets (utf8-octets text)))
     (expect (multiple-value-list
              (cl-regex-kit::utf8-character-at octets 0))
             :to-equal
             (list (char text 0) (length octets) t))
     (expect (octets-to-string octets :encoding :utf-8)
             :to-equal
             text))))

(it-utf8-decode-at-cases
 "rejects malformed UTF-8 byte sequences when decoding forward"
 (((192 128) 0 (nil))
  ((128) 0 (nil))
  ((194) 0 (nil))
  ((195 40) 0 (nil))
  ((224 159 128) 0 (nil))
  ((226 130) 0 (nil))
  ((226 40 129) 0 (nil))
  ((237 160 128) 0 (nil))
  ((240 143 128 128) 0 (nil))
  ((240 159 152) 0 (nil))
  ((240 159 40 128) 0 (nil))
  ((240 159 152 40) 0 (nil))
  ((244 144 128 128) 0 (nil))
 ((245 128 128 128) 0 (nil))
  ((97) 1 (nil))))

(it
 "preserves UTF-8 non-boundary metadata across forward and backward decoding"
 (expect (multiple-value-list
          (cl-regex-kit::utf8-character-before (utf8-octets "é") 2))
         :to-equal
         (list (code-char #x00e9) 0 t))
 (let ((text (utf8-octets "aé")))
   (expect (cl-regex-kit::byte-unicode-non-boundary-position-p text 1)
           :to-be-truthy)
   (expect (cl-regex-kit::byte-unicode-non-boundary-position-p text 2)
           :to-be-null)
   (expect (cl-regex-kit::byte-unicode-non-boundary-position-p text 3)
           :to-be-truthy)))

(it
 "decodes UTF-8 scalars backward without accepting malformed boundaries"
 (dolist (text '("A" "é" "☃" "😀"))
   (let ((octets (utf8-octets text)))
     (expect (multiple-value-list
              (cl-regex-kit::utf8-character-before octets (length octets)))
             :to-equal
             (list (char text 0) 0 t))
     (expect (octets-to-string octets :encoding :utf-8)
             :to-equal
             text)))
 (dolist (case '(((128) 1 (nil nil nil))
  ((194) 1 (nil nil nil))
  ((195 169) 1 (nil nil nil))
  ((97 128) 2 (nil nil nil))
  ((226 130) 2 (nil nil nil))
  ((240 159 152) 3 (nil nil nil))))
   (destructuring-bind (octets-values position expected) case
     (expect (multiple-value-list
              (cl-regex-kit::utf8-character-before
               (apply #'octets octets-values)
               position))
             :to-equal
             expected))))

(it
 "rejects out-of-range backward UTF-8 decoding positions"
 (let ((text (octets #x61 #xc3 #xa9)))
   (expect (multiple-value-list
            (cl-regex-kit::utf8-character-before text 0))
           :to-equal
           (list nil nil nil))
   (expect (multiple-value-list
            (cl-regex-kit::utf8-character-before text 4))
           :to-equal
           (list nil nil nil))))


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
    (signals error (cl-regex-kit::matcher-matches-p '(:unknown) #\a))))

(it-each (((:ranges ((#\a . #\c))) 98 t)
          ((:union (:ranges ((#\a . #\c)))) 98 t)
          ((:intersection (:ranges ((#\a . #\c))) (:ranges ((98 . 99)))) 98 t)
          ((:difference (:ranges ((#\a . #\c))) (:ranges ((98 . 98)))) 97 t)
          ((:symmetric-difference (:ranges ((#\a . #\c))) (:ranges ((98 . 100)))) 97 t)
          ((:negate (:ranges ((#\a . #\c)))) 122 t)
          ((:unknown) 97 nil))
    "evaluates byte matcher ~S against octet ~D"
    (matcher octet expected)
  (expect (not (null (cl-regex-kit::byte-matcher-matches-p matcher octet)))
          :to-equal expected))

(it "handles byte-class folding, boundaries, and CRLF positions"
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
    (expect (cl-regex-kit::byte-line-end-p crlf 2 t #\Newline) :to-be-null)))

(it "distinguishes byte and Unicode boundary edge cases"
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
    (expect (cl-regex-kit::line-end-p text 2 t #\Newline) :to-be-null)))

(it "keeps string and UTF-8 octet boundary domains distinct at input edges"
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
            :to-be-null)))
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
    (expect "α" :to-match-regex class-property)))

(it-each (("Lu" #\A t)
          ("Lu" #\a nil)
          ("Nd" #.(code-char #xff11) t)
          ("White_Space" #.(code-char #x3000) t)
          ("Greek" #\α t))
    "executes pre-resolved Unicode descriptor ~A for ~S"
    (name character expected)
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
      expected)))

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
(it-unicode-property-equivalence-cases
 "treats representative Unicode property aliases equivalently"
 (("Lu" "General_Category=Uppercase_Letter" #\A)
  ("isLu" "Lu" #\A)
  ("sc=Grek" "Script=Greek" #\α)
  ("Script=isGreek" "Script=Greek" #\α)
  ("Grek" "Greek" #\α)
  ("isGrek" "Grek" #\α)
  ("Latn" "Latin" #\A)
  ("Cyrl" "Cyrillic" #.(code-char #x0414))
  ("scx=Grek" "Script_Extensions=Greek" #\α)
  ("Script_Extensions=isGreek" "Script_Extensions=Greek" #\α)
  ("blk=Basic_Latin" "InBasic_Latin" #\A)
  ("Block=isBasic_Latin" "Block=Basic_Latin" #\A)
  ("age=1.1" "Age=V1_1" #\A)
  ("Age=isV1_1" "Age=V1_1" #\A)
  ("GCB=EX" "Grapheme_Cluster_Break=Extend" #.(code-char #x0301))
  ("GCB=isExtend" "GCB=Extend" #.(code-char #x0301))
  ("WB=LE" "Word_Break=ALetter" #\a)
  ("WB=isALetter" "WB=ALetter" #\a)
  ("SB=UP" "Sentence_Break=Upper" #\A)
  ("SB=isUpper" "Sentence_Break=Upper" #\A)
  ("isWhite_Space" "White_Space" #\Space)))
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
(it "applies UAX #29 word-boundary rules" (let ((hebrew-single (format nil "~C~C~C" (code-char #x05d0) (code-char #x0027) (code-char #x05d1))) (hebrew-double (format nil "~C~C~C" (code-char #x05d0) (code-char #x0022) (code-char #x05d1))) (regional (format nil "~C~C~C" (code-char #x1f1e6) (code-char #x1f1e7) (code-char #x1f1e8))) (zwj-emoji (format nil "~C~C~C" (code-char #x1f469) (code-char #x200d) (code-char #x1f4bb)))) (expect (cl-regex-kit::word-boundary-p "a:b" 1 t) :to-be-null) (expect (cl-regex-kit::word-boundary-p "a:b" 2 t) :to-be-null) (expect (cl-regex-kit::word-boundary-p "1,234" 1 t) :to-be-null) (expect (cl-regex-kit::word-boundary-p "1,234" 2 t) :to-be-null) (expect (cl-regex-kit::word-boundary-p "a-b" 1 t) :to-be-truthy) (expect (cl-regex-kit::word-boundary-p "a-b" 2 t) :to-be-truthy) (expect (cl-regex-kit::word-boundary-p "  " 1 t) :to-be-null) (expect (cl-regex-kit::word-boundary-p hebrew-single 1 t) :to-be-null) (expect (cl-regex-kit::word-boundary-p hebrew-single 2 t) :to-be-null) (expect (cl-regex-kit::word-boundary-p hebrew-double 1 t) :to-be-null) (expect (cl-regex-kit::word-boundary-p hebrew-double 2 t) :to-be-null) (expect (cl-regex-kit::word-boundary-p regional 1 t) :to-be-null) (expect (cl-regex-kit::word-boundary-p regional 2 t) :to-be-truthy) (expect (cl-regex-kit::word-boundary-p zwj-emoji 2 t) :to-be-null)))
(it "covers newline and significant-context word boundaries"
  (let ((crlf (format nil "~C~C" #\Return #\Linefeed))
        (newline (format nil "~C~C" #\Linefeed #\a))
        (ignored-left (format nil "~C~C" (code-char #x0308) #\a))
        (numeric-letter (format nil "~C~C" (code-char #x06dd)
                                (code-char #x070f)))
        (katakana (format nil "~C~C" (code-char #x30ab)
                          (code-char #x30ad))))
    (expect (cl-regex-kit::word-boundary-p crlf 1 t) :to-be-null)
    (expect (cl-regex-kit::word-boundary-p newline 1 t) :to-be-truthy)
    (expect (cl-regex-kit::word-boundary-p ignored-left 1 t)
            :to-be-truthy)
    (expect (cl-regex-kit::word-boundary-p numeric-letter 1 t)
            :to-be-null)
    (expect (cl-regex-kit::word-boundary-p katakana 1 t) :to-be-null)
    (expect (cl-regex-kit::word-boundary-p "a_" 1 t) :to-be-null)
    (expect (cl-regex-kit::word-boundary-p "_a" 1 t) :to-be-null)))
(it
  "uses Unicode 16.0 Word_Break data for boundary decisions"
  (dolist (case (quote ((#x06dd :NUMERIC)
                        (#x070f :ALETTER)
                        (#x0308 :EXTEND))))
    (destructuring-bind (code expected) case
      (expect (cl-regex-kit::unicode-word-break-class (code-char code))
              :to-be
              expected)))
  (let ((space-extend-space
          (format nil "~C~C~C"
                  (code-char #x0020)
                  (code-char #x0308)
                  (code-char #x0020))))
    (expect (cl-regex-kit::word-boundary-p space-extend-space 2 t)
            :to-be-truthy))
  (expect
    (cl-regex-kit::word-boundary-p
      (format nil "~C~C" (code-char #x06dd) (code-char #x0661))
      1
      t)
    :to-be-null)
  (expect
    (cl-regex-kit::word-boundary-p
      (format nil "~C~C" (code-char #x070f) (code-char #x071d))
      1
      t)
    :to-be-null)
  (expect
    (cl-regex-kit::unicode-property-descriptor-matches-p
      (cl-regex-kit::resolve-unicode-property "WB=Numeric")
      (code-char #x06dd))
    :to-be-truthy)
  (expect
    (cl-regex-kit::unicode-property-descriptor-matches-p
      (cl-regex-kit::resolve-unicode-property "WB=ALetter")
      (code-char #x070f))
    :to-be-truthy))
