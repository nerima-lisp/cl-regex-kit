(in-package #:cl-regex-kit/test)

(it
 "supports RE2 literal and never-capture compilation options"
 (let* ((pattern "a b#c(d)")
        (regex (compile-regex pattern :literal t :ignore-whitespace t)))
   (expect pattern :to-match-regex regex)
   (expect-not "abc" :to-match-regex regex))
 (expect
  "A.B"
  :to-match-regex
  (compile-regex "a.b" :literal t :case-insensitive t))
 (let* ((regex (compile-regex "(a)(?<named>b)(?:c)" :never-capture t))
        (result (scan regex "abc")))
   (expect (regex-group-count regex) :to-equal 1)
   (expect (regex-capture-count regex) :to-equal 2)
   (unless (equalp (regex-capture-names regex) #(nil "named"))
     (error "NEVER-CAPTURE changed named capture metadata"))
   (unless (equalp (match-captures result "abc") #("abc" "b"))
     (error "NEVER-CAPTURE retained an unnamed capture")))
 (expect
  (regex-set-match-p (compile-regex-set '("a.b") :literal t) "a.b")
  :to-be-truthy)
 (signals type-error (compile-regex "a" :literal :enabled))
 (signals type-error (compile-byte-regex "a" :never-capture :enabled)))

(it
 "supports RE2 never-newline compilation mode across regex and regex sets"
 (let ((newline (string #\Newline)))
   (expect (scan (compile-regex "(?s:.)" :never-newline t) newline) :to-be nil)
   (expect (scan (compile-regex newline :never-newline t) newline) :to-be nil)
   (expect (scan (compile-regex "\\012" :never-newline t) newline) :to-be nil)
   (expect (scan (compile-regex "\\R" :never-newline t) newline) :to-be nil)
   (expect
    (scan
     (compile-regex "\\R" :never-newline t)
     (format nil "~C~C" #\Return #\Newline))
    :to-be
    nil)
   (expect
    (scan (compile-regex "[[:space:]]" :never-newline t) newline)
    :to-be
    nil)
   (expect (scan (compile-regex "(?s:.)" :never-newline t) "x") :to-be-truthy)
   (expect
    (regex-set-matches
     (compile-regex-set (quote ("(?s:.)" "x")) :never-newline t)
     newline)
    :to-equal
    nil)
   (expect
    (regex-set-matches
     (compile-regex-set (quote ("\\R" "x")) :never-newline t)
     newline)
    :to-equal
    nil))
 (expect
  (scan (compile-byte-regex "\\C" :never-newline t) (octets #x0a))
  :to-be
  nil)
 (expect
  (scan (compile-byte-regex "\\x0A" :never-newline t) (octets #x0a))
  :to-be
  nil)
 (expect
  (scan (compile-byte-regex "\\R" :never-newline t) (octets #x0a))
  :to-be
  nil)
 (expect
  (scan
   (compile-byte-regex "\\R" :never-newline t)
   (octets #x0d #x0a))
  :to-be
  nil)
 (expect
  (scan (compile-byte-regex "\\C" :never-newline t) (octets #x41))
  :to-be-truthy)
 (expect
  (regex-set-match-p
   (compile-byte-regex-set (quote ("\\C")) :never-newline t)
   (octets #x0a))
  :to-be
  nil)
 (signals type-error (compile-regex "a" :never-newline :enabled)))

(it
 "supports CRLF and custom line terminators without matching inside CRLF"
 (let ((text (format nil "~C~Cfoo~C~C" #\Return #\Newline #\Return #\Newline))
       (cr (string #\Return))
       (crlf (format nil "~C~C" #\Return #\Newline)))
   (expect (match-string (match "(?mR)^foo$" text) text) :to-equal "foo")
   (expect (match "(?mR)^\\n" crlf) :to-be nil)
   (expect (match "." cr) :not :to-be nil)
   (expect (scan (compile-regex "." :crlf t) cr) :to-be nil))
 (let ((text ";foo;bar"))
   (expect
    (match-string
     (scan (compile-regex "^foo$" :multi-line t :line-terminator #\;) text)
     text)
    :to-equal
    "foo"))
 (let* ((text
         (make-array
          9
          :element-type
          '(unsigned-byte 8)
          :initial-contents
          '(59 102 111 111 59 98 97 114 59)))
        (result
         (scan
          (compile-byte-regex "^foo$" :multi-line t :line-terminator #\;)
          text)))
   (expect-match-span-cases
    (result 1 4)))
 (let ((regex
        (compile-byte-regex "^foo$" :multi-line t :line-terminator #xff)))
   (expect (scan regex (octets #xff 102 111 111 #xff)) :to-be-truthy)
   (expect
    (regex-set-matches
     (compile-byte-regex-set '("^foo$") :multi-line t :line-terminator #xff)
     (octets #xff 102 111 111 #xff))
    :to-equal
    '(0)))
 (let ((regex (compile-byte-regex "(?-u:.)" :line-terminator #xff)))
   (expect (scan regex (octets #xff)) :to-be-null)
   (expect (scan regex (octets #x80)) :to-be-truthy))
 (signals type-error (compile-regex "." :line-terminator #xff))
 (signals type-error (compile-byte-regex "." :line-terminator 256)))

(it
 "supports all POSIX classes, class set operations, and extended mode"
 (dolist (case '(("alnum" . #\7)
                 ("alpha" . #\A)
                 ("ascii" . #\A)
                 ("blank" . #\Space)
                 ("cntrl" . #\Newline)
                 ("digit" . #\7)
                 ("graph" . #\!)
                 ("lower" . #\a)
                 ("print" . #\Space)
                 ("punct" . #\!)
                 ("space" . #\Tab)
                 ("upper" . #\A)
                 ("word" . #\_)
                 ("xdigit" . #\F)))
   (destructuring-bind (name . character) case
     (expect
      (match (format nil "[[:~A:]]" name) (string character))
      :to-be-truthy)))
 (expect-match-string-cases
  ((match "[[:alpha:]]+" "42abc") "42abc" "abc")
  ((match "[[:alpha:]]+" "éabc") "éabc" "abc"))
 (expect (match "[[:digit:]]" (string (code-char #xff11))) :to-be nil)
 (expect-match-string-cases
  ((match "[[:graph:]]+" " x!~ ") " x!~ " "x!~"))
 (let ((text (format nil "~C x!~~~C" #\Tab #\Newline)))
   (expect-match-string-cases
    ((match "[[:print:]]+" text) text " x!~")))
 (expect-match-string-cases
  ((match "[[:punct:]]+" "a!@[]_`{}~z") "a!@[]_`{}~z" "!@[]_`{}~")
  ((match "[[:^digit:]]+" "123abc456") "123abc456" "abc")
  ((match "[a-z&&[^aeiou]]+" "a-bcdf-e") "a-bcdf-e" "bcdf")
  ((match "[a-z&&aeiou]+" "x-aeiou-z") "x-aeiou-z" "aeiou")
  ((match "[a-z--[aeiou]]+" "a-bcdf-e") "a-bcdf-e" "bcdf")
  ((match "[a-z--aeiou]+" "a-bcdf-e") "a-bcdf-e" "bcdf")
  ((match "[a-f~~[d-z]]+" "abcgyz") "abcgyz" "abcgyz")
  ((match "[a-f~~d-z]+" "abcgyz") "abcgyz" "abcgyz")
  ((match "[a-z--aeiou~~x]+" "a-bcdx-f") "a-bcdx-f" "bcd"))
 (let ((pattern (format nil "(?x) a # ignored~% b")))
   (expect-match-string-cases
    ((match pattern "--ab--") "--ab--" "ab")))
 (let ((pattern (format nil "(?x)[ a # ignored~% b ]+")))
   (expect-match-string-cases
    ((match pattern "--a#b--") "--a#b--" "a")))
 (expect-match-string-cases
  ((match "(?x)[ a - c ]+" "--abc--") "--abc--" "abc")
  ((match "(?x)[ a - z & & [ ^ a e i o u ] ]+" "a-bcdf-e")
   "a-bcdf-e"
   "bcdf")
  ((match "(?x)a{ 2 }" "--aa--") "--aa--" "aa")
  ((match "(?x)\\x{ 6 1 }+" "--aaa--") "--aaa--" "aaa")
  ((match "(?x)\\p{ L e t t e r }+" "--abc--") "--abc--" "abc")
  ((match "(?x)\\b{ s t a r t }foo" " foo food") " foo food" "foo"))
 (let ((pattern (format nil "(?x)\\b{ start # ignored~% }foo")))
   (expect-match-string-cases
    ((match pattern " foo food") " foo food" "foo")))
 (expect-match-string-cases
  ((match "\\b{2}" " foo") " foo" "")
  ((match "(?x)[\\ ]+" "a  b") "a  b" "  ")
  ((match "(?x)[\\#]+" "a##b") "a##b" "##")))

(it
 "provides compiled literals, boolean predicates, and bounded repetition"
 (let ((compiled (cl-regex-kit:regex "\\d+")))
   (expect (regex-p compiled) :to-be-truthy)
   (expect "x42" :to-match-regex compiled)
   (expect-not "abc" :to-match-regex compiled)
   (expect (cl-regex-kit:full-match-p compiled "42") :to-be-truthy)
   (expect (cl-regex-kit:full-match-p compiled "x42") :to-be nil)
   (expect
    (cl-regex-kit:full-match-p compiled "id=42!" :start 3 :end 5)
    :to-be-truthy)
   (expect
    (cl-regex-kit:full-match-p compiled "id=42!" :start 3 :end 6)
    :to-be
    nil))
 (let ((compiled (cl-regex-kit:regex "cat" :case-insensitive t)))
   (expect "CAT" :to-match-regex compiled))
 (let ((compiled (cl-regex-kit:byte-regex "cat" :case-insensitive t))
       (text
        (make-array
         3
         :element-type
         '(unsigned-byte 8)
         :initial-contents
         '(67 65 84))))
   (expect text :to-match-regex compiled))
 (let ((compiled (cl-regex-kit:byte-regex "ab"))
       (text
        (make-array
         4
         :element-type
         '(unsigned-byte 8)
         :initial-contents
         '(120 97 98 121))))
   (expect
    (cl-regex-kit:full-match-p compiled text :start 1 :end 3)
    :to-be-truthy)
   (expect (cl-regex-kit:full-match-p compiled text :start 1 :end 4) :to-be nil))
(expect-macroexpand-signals-cases
 error
 (cl-regex-kit:regex "cat" :case-insensitive enabled)
 (cl-regex-kit:byte-regex "cat" case-insensitive t)
 (cl-regex-kit:byte-regex "cat" :case-insensitive enabled))
 (signals regex-syntax-error (compile-regex "a{1001}")))

(it
 "preserves finite-automaton edge semantics across a table of cases"
 (expect-match-string-cases
  ((match "(?i-u:[a-z]+)" "AbC") "AbC" "AbC")
  ((match "(?-u:\\w+)" "éclair") "éclair" "clair")
  ((match "(?:a|ab)" "ab") "ab" "a")
  ((match "a*?" "aaa") "aaa" "")
  ((match "\\Bcat\\B" "scatter") "scatter" "cat")
  ((match "\\b{start}\\w+\\b{end}" "--word--") "--word--" "word")))
