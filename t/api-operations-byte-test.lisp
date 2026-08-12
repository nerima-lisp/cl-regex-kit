(in-package #:cl-regex-kit/test)

(progn
  (it
   "matches octet vectors through the byte regex API"
   (let* ((text (octets #xff #x61 #x0a #x62 #x80))
          (regex (compile-byte-regex "\\C(a)\\C"))
          (result (scan regex text)))
     (expect (byte-regex-p regex) :to-be-truthy)
     (expect (match-start result) :to-equal 0)
     (expect (match-end result) :to-equal 3)
     (expect
      (coerce (match-group-string result 1 text) 'list)
      :to-equal
      '(97))
     (expect (scan (compile-byte-regex "(?s:.)") text) :to-be-truthy)
     (expect (scan (compile-byte-regex "\\bA") (octets 255 65)) :to-be-truthy)
     (expect
      (mapcar
       (lambda (field)
         (coerce field 'list))
       (split (compile-byte-regex "\\C") (octets 65 66)))
      :to-equal
      '(nil nil nil))
     (expect
      (mapcar
       (lambda (field)
         (coerce field 'list))
       (split-n (compile-byte-regex "\\C") (octets 65 66) 2))
      :to-equal
      '(nil (66)))
     (expect
      (coerce
       (replace-first
        (compile-byte-regex "(A)")
        (octets #xff 65 #x80)
        (octets #x24 #x31 #x21))
       'list)
      :to-equal
      '(255 65 33 128))
     (expect
      (coerce
       (replace-all (compile-byte-regex "A") (octets 65 66 65) (octets 0))
       'list)
      :to-equal
      '(0 66 0))
     (expect
      (coerce
       (replace-first
        (compile-byte-regex "(?<letter>A)")
        (octets 65)
        (octets #x24 #x7b #x6c #x65 #x74 #x74 #x65 #x72 #x7d))
       'list)
      :to-equal
      '(65))
     (expect
      (coerce
       (replace-first
        (compile-byte-regex "A")
        (octets 65)
        (lambda (result source)
          (declare (ignore result source))
          (octets #xff)))
       'list)
      :to-equal
      '(255))
     (signals
      type-error
      (replace-first (compile-byte-regex "A") (octets 65) "A"))
     (signals type-error (scan regex "a"))
     (signals type-error (scan (compile-regex "a") (octets 97)))
     (let* ((unicode-text (octets #xff #xc3 #xa9 #xfe))
            (unicode-letter (scan (compile-byte-regex "\\p{L}") unicode-text))
            (mixed
             (scan
              (compile-byte-regex "(?-u:\\xFF)\\p{L}(?-u:\\xFE)")
              unicode-text)))
       (expect (match-start unicode-letter) :to-equal 1)
       (expect (match-end unicode-letter) :to-equal 3)
       (expect (match-start mixed) :to-equal 0)
       (expect (match-end mixed) :to-equal 4)
       (expect
        (match-start (scan (compile-byte-regex ".") unicode-text))
        :to-equal
        1)
       (expect
        (match-end (scan (compile-byte-regex ".") unicode-text))
        :to-equal
        3)
       (expect
        (match-start (scan (compile-byte-regex "(?-u:.)") unicode-text))
        :to-equal
        0)
       (expect (scan (compile-byte-regex "(?u:a)") (octets 97)) :to-be-truthy))
     (signals regex-syntax-error (compile-byte-regex "(?-u:\\p{L})"))
     (signals regex-syntax-error (compile-byte-regex "(?-u:[\\p{L}])"))
     (expect
      (scan
      (compile-byte-regex "(?mR)^\\n")
       (ascii-octets (format nil "~C~C" #\Return #\Linefeed)))
      :to-be
      nil)))
  (it-byte-match-string-cases
   "matches CR/LF-sensitive multiline byte anchors through declarative byte case tables"
   ("(?mR)^B" (format nil "A~C~CB" #\Return #\Linefeed) "B")
   ("(?mR)A$" (format nil "A~C~CB" #\Return #\Linefeed) "A")
   ("(?mR)^B" (format nil "A~CB" #\Return) "B")
   ("(?mR)A$" (format nil "A~CB" #\Return) "A")
   ("(?mR)^B" (format nil "A~CB" #\Linefeed) "B")
   ("(?mR)A$" (format nil "A~CB" #\Linefeed) "A"))
  (it-byte-ascii-match-string-cases
   "matches ASCII-oriented byte constructs through declarative byte case tables"
   ("(?-u:[a-z&&[^aeiou]]+)" "aeiouxyz" "xyz")
   ("(?-u:[a-z&&aeiou]+)" "aeiouxyz" "aeiou")
   ("(?-u:[a-z--aeiou]+)" "aeiouxyz" "xyz")
   ("(?-u:[a-f~~d-z]+)" "abcdefgh" "abc")
   ("(?-u:\\d+)" "abc123" "123")
   ("(?-u:[[:alpha:]]+)" "A1" "A")
   ("(?-u:(?i:[a-z])+)" "ABC" "ABC")
   ("(?-u:[^a])" "aB" "B")
   ("(?-u:\\bcat\\b)" " cat " "cat")
   ("(?-u:\\Bcat\\B)" "scatx" "cat")
   ("(?-u:\\b{start}cat)" " cat" "cat")
   ("(?-u:cat\\b{end})" "cat " "cat")
   ("(?-u:\\b{start-half}cat)" "!!cat" "cat")
   ("(?-u:cat\\b{end-half})" "cat!!" "cat"))
  (it-byte-ascii-match-string-cases
   "matches Unicode-aware byte boundaries through declarative byte case tables"
   ("\\b{start}cat" " cat" "cat")
   ("cat\\b{end}" "cat " "cat")
   ("\\b{start-half}cat" "!!cat" "cat")
   ("cat\\b{end-half}" "cat!!" "cat")
   ("\\Bcat\\B" "scatx" "cat")
   ("\\b\\p{L}+\\b" " cafe " "cafe"))
  (it
   "keeps UTF-8 scalar and Unicode-boundary semantics in byte regexes"
   (let* ((text
           (octets #x20 #xc3 #xa9 #xe2 #x82 #xac #xf0 #x9f #x98 #x80 #x20))
          (dot-matches (all-matches (compile-byte-regex ".") text))
          (word (scan (compile-byte-regex "\\b{start}\\p{L}+\\b{end}") text))
          (start-half
           (scan (compile-byte-regex "\\b{start-half}\\p{L}") text))
          (end-half (scan (compile-byte-regex "\\p{L}\\b{end-half}") text)))
     (expect
      (mapcar
       (lambda (result)
         (list (match-start result) (match-end result)))
       dot-matches)
      :to-equal
      '((0 1) (1 3) (3 6) (6 10) (10 11)))
     (expect (match-start word) :to-equal 1)
     (expect (match-end word) :to-equal 3)
     (expect (match-start start-half) :to-equal 1)
     (expect (match-end start-half) :to-equal 3)
     (expect (match-start end-half) :to-equal 1)
     (expect (match-end end-half) :to-equal 3)
     (expect
      (scan (compile-byte-regex "\\b{start}\\p{L}") text :start 2)
      :to-be
      nil)
     (expect-falsy-cases
       (scan (compile-byte-regex "\\p{L}") (octets #xc0 #x80))
       (scan (compile-byte-regex "\\p{L}") (octets #x80))
       (scan (compile-byte-regex "\\p{L}") (octets #xc2))
       (scan (compile-byte-regex "\\p{L}") (octets #xe0 #x80 #x80))
       (scan (compile-byte-regex "\\p{L}") (octets #xe2 #x82))
       (scan (compile-byte-regex "\\p{L}") (octets #xed #xa0 #x80))
       (scan (compile-byte-regex "\\p{L}") (octets #xf0 #x80 #x80 #x80))
       (scan (compile-byte-regex "\\p{L}") (octets #xf0 #x9f #x98))
       (scan (compile-byte-regex "\\p{L}") (octets #xf4 #x90 #x80 #x80)))
     (let ((scalar (octets #xc3 #xa9)))
       (expect
        (scan (compile-byte-regex "\\b") scalar :start 1 :end 1)
        :to-be
        nil)
       (expect
        (scan (compile-byte-regex "\\B") scalar :start 1 :end 1)
        :to-be
        nil))))
  (it
   "applies UAX #29 word boundaries at UTF-8 scalar offsets"
   (let ((punctuated (octets #x61 #x3a #x62))
         (regional
           (octets
            #xf0 #x9f #x87 #xa6
            #xf0 #x9f #x87 #xa7
            #xf0 #x9f #x87 #xa8)))
     (expect (cl-regex-kit::byte-unicode-word-boundary-p punctuated 1) :to-be-null)
     (expect (cl-regex-kit::byte-unicode-word-boundary-p punctuated 2) :to-be-null)
     (expect (cl-regex-kit::byte-unicode-word-boundary-p regional 4) :to-be-null)
     (expect (cl-regex-kit::byte-unicode-word-boundary-p regional 8) :to-be-truthy))))
