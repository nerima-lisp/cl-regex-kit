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
 (flet ((octets (&rest values)
          (make-array
           (length values)
           :element-type
           (quote (unsigned-byte 8))
           :initial-contents
           values)))
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
    nil))
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
   (expect (match-start result) :to-equal 1)
   (expect (match-end result) :to-equal 4))
 (flet ((octets (&rest values)
          (make-array
           (length values)
           :element-type
           '(unsigned-byte 8)
           :initial-contents
           values)))
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
     (expect (scan regex (octets #x80)) :to-be-truthy)))
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
 (expect (match-string (match "[[:alpha:]]+" "42abc") "42abc") :to-equal "abc")
 (expect (match-string (match "[[:alpha:]]+" "éabc") "éabc") :to-equal "abc")
 (expect (match "[[:digit:]]" (string (code-char #xff11))) :to-be nil)
 (expect (match-string (match "[[:graph:]]+" " x!~ ") " x!~ ") :to-equal "x!~")
 (let ((text (format nil "~C x!~~~C" #\Tab #\Newline)))
   (expect (match-string (match "[[:print:]]+" text) text) :to-equal " x!~"))
 (expect
  (match-string (match "[[:punct:]]+" "a!@[]_`{}~z") "a!@[]_`{}~z")
  :to-equal
  "!@[]_`{}~")
 (expect
  (match-string (match "[[:^digit:]]+" "123abc456") "123abc456")
  :to-equal
  "abc")
 (expect
  (match-string (match "[a-z&&[^aeiou]]+" "a-bcdf-e") "a-bcdf-e")
  :to-equal
  "bcdf")
 (expect
  (match-string (match "[a-z&&aeiou]+" "x-aeiou-z") "x-aeiou-z")
  :to-equal
  "aeiou")
 (expect
  (match-string (match "[a-z--[aeiou]]+" "a-bcdf-e") "a-bcdf-e")
  :to-equal
  "bcdf")
 (expect
  (match-string (match "[a-z--aeiou]+" "a-bcdf-e") "a-bcdf-e")
  :to-equal
  "bcdf")
 (expect
  (match-string (match "[a-f~~[d-z]]+" "abcgyz") "abcgyz")
  :to-equal
  "abcgyz")
 (expect
  (match-string (match "[a-f~~d-z]+" "abcgyz") "abcgyz")
  :to-equal
  "abcgyz")
 (expect
  (match-string (match "[a-z--aeiou~~x]+" "a-bcdx-f") "a-bcdx-f")
  :to-equal
  "bcd")
 (let ((pattern (format nil "(?x) a # ignored~% b")))
   (expect (match-string (match pattern "--ab--") "--ab--") :to-equal "ab"))
 (let ((pattern (format nil "(?x)[ a # ignored~% b ]+")))
   (expect (match-string (match pattern "--a#b--") "--a#b--") :to-equal "a"))
 (expect
  (match-string (match "(?x)[ a - c ]+" "--abc--") "--abc--")
  :to-equal
  "abc")
 (expect
  (match-string
   (match "(?x)[ a - z & & [ ^ a e i o u ] ]+" "a-bcdf-e")
   "a-bcdf-e")
  :to-equal
  "bcdf")
 (expect (match-string (match "(?x)a{ 2 }" "--aa--") "--aa--") :to-equal "aa")
 (expect
  (match-string (match "(?x)\\x{ 6 1 }+" "--aaa--") "--aaa--")
  :to-equal
  "aaa")
 (expect
  (match-string (match "(?x)\\p{ L e t t e r }+" "--abc--") "--abc--")
  :to-equal
  "abc")
 (expect
  (match-string (match "(?x)\\b{ s t a r t }foo" " foo food") " foo food")
  :to-equal
  "foo")
 (let ((pattern (format nil "(?x)\\b{ start # ignored~% }foo")))
   (expect
    (match-string (match pattern " foo food") " foo food")
    :to-equal
    "foo"))
 (expect (match-string (match "\\b{2}" " foo") " foo") :to-equal "")
 (expect (match-string (match "(?x)[\\ ]+" "a  b") "a  b") :to-equal "  ")
 (expect (match-string (match "(?x)[\\#]+" "a##b") "a##b") :to-equal "##"))

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
 (signals
  error
  (macroexpand-1 '(cl-regex-kit:regex "cat" :case-insensitive enabled)))
 (signals
  error
  (macroexpand-1 '(cl-regex-kit:byte-regex "cat" case-insensitive t)))
 (signals
  error
  (macroexpand-1 '(cl-regex-kit:byte-regex "cat" :case-insensitive enabled)))
 (signals regex-syntax-error (compile-regex "a{1001}")))

(it
 "compiles immutable regex sets and reports every matching pattern index"
 (dolist (case '((("cat" "dog" "\\d+") "dog42" 0 (1 2))
                 (("cat" "dog" "\\d+") "bird" 0 nil)
                 (("a" "a" "^z") "a" 0 (0 1))
                 (("a") "za" 2 nil)
                 (() "anything" 0 nil)))
   (destructuring-bind (patterns text start expected) case
     (let ((set (compile-regex-set patterns)))
       (unless (equal (regex-set-matches set text :start start) expected)
         (error
          "Set ~S on ~S: expected ~S, got ~S"
          patterns
          text
          expected
          (regex-set-matches set text :start start)))
       (unless (eq
                (not (null expected))
                (regex-set-match-p set text :start start))
         (error "Set predicate disagreed for ~S on ~S" patterns text)))))
 (let* ((source (vector "cat" "dog"))
        (set (compile-regex-set source)))
   (setf (aref source 0) "bird")
   (unless (equalp (regex-set-patterns set) #("cat" "dog"))
     (error "REGEX-SET retained an alias to its input patterns"))
   (setf (aref (regex-set-patterns set) 0) "bird")
   (unless (equalp (regex-set-patterns set) #("cat" "dog"))
     (error "REGEX-SET exposed mutable source patterns")))
 (dolist (compiler (list #'compile-regex #'compile-byte-regex))
   (let* ((source (copy-seq "cat"))
          (compiled (funcall compiler source)))
     (setf (char source 0) #\b)
     (unless (string= (cl-regex-kit:regex-source compiled) "cat")
       (error "REGEX retained an alias to its input source"))
     (let ((exposed (cl-regex-kit:regex-source compiled)))
       (setf (char exposed 0) #\r)
       (unless (string= (cl-regex-kit:regex-source compiled) "cat")
         (error "REGEX exposed mutable source text")))))
 (dolist (compiler (list #'compile-regex-set #'compile-byte-regex-set))
   (let* ((source (vector (copy-seq "cat") (copy-seq "dog")))
          (set (funcall compiler source)))
     (setf (char (aref source 0) 0) #\b)
     (unless (equalp (regex-set-patterns set) #("cat" "dog"))
       (error "REGEX-SET retained aliases to input source strings"))
     (let ((exposed (regex-set-patterns set)))
       (setf (char (aref exposed 0) 0) #\r)
       (unless (equalp (regex-set-patterns set) #("cat" "dog"))
         (error "REGEX-SET exposed mutable source strings")))))
 (let ((set (regex-set "cat" "dog")))
   (unless (equal (regex-set-matches set "a dog") '(1))
     (error "REGEX-SET macro did not compile its literal patterns")))
 (let ((set (regex-set "cat" :case-insensitive t)))
   (unless (equal (regex-set-matches set "CAT") '(0))
     (error "REGEX-SET macro did not compile literal builder options")))
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
   (unless (equal (regex-set-matches set crlf-text) '(0))
     (error "REGEX-SET did not propagate builder options to merged patterns"))))

(it
 "keeps merged regex-set execution equivalent to member regex scans"
 (dolist (case '((("(?<letter>a)" "b") "a" 0)
                 (("a*" "^$" "\\A\\z") "" 0)
                 (("(?i)cat" "\\p{Greek}+") "Cat α" 0)
                 (("(?i)k" "\\w+" "(?s:.)") "K\r\n" 0)
                 (("\\Afoo\\z" "o+") "foo" 1)
                 (("(?m)^foo$" "bar") "x\nfoo\ny" 0)
                 (("\\b{start-half}\\w+" "\\b{end-half}\\w+") "α" 0)
                 (("a" "a") "a" 0)))
   (destructuring-bind (patterns text start) case
     (let ((expected
            (loop for pattern in patterns
                  for index from 0
                  when (scan (compile-regex pattern) text :start start)
                    collect index))
           (actual
            (regex-set-matches (compile-regex-set patterns) text :start start)))
       (unless (equal actual expected)
         (error
          "Merged set differed from member scans: ~S on ~S from ~D: ~S /= ~S"
          patterns
          text
          start
          actual
          expected))))))

(it
 "keeps larger merged regex sets equivalent to member regex scans"
 (let* ((patterns
         (loop for index below 32
               collect (format nil "(?:item~D|code~D)(?:-x)?" index index)))
        (text "prefix code17-x middle item3 suffix")
        (expected
         (loop for pattern in patterns
               for index from 0
               when (scan (compile-regex pattern) text)
                 collect index))
        (actual (regex-set-matches (compile-regex-set patterns) text)))
   (unless (equal actual expected)
     (error
      "Large merged set differed from member scans: ~S /= ~S"
      actual
      expected))))

(it
 "preserves finite-automaton edge semantics across a table of cases"
 (dolist (case '(("(?i-u:[a-z]+)" "AbC" "AbC")
                 ("(?-u:\\w+)" "éclair" "clair")
                 ("(?:a|ab)" "ab" "a")
                 ("a*?" "aaa" "")
                 ("\\Bcat\\B" "scatter" "cat")
                 ("\\b{start}\\w+\\b{end}" "--word--" "word")))
   (destructuring-bind (pattern text expected) case
     (let ((actual (match-string (match pattern text) text)))
       (unless (string= actual expected)
         (error
          "Pattern ~S on ~S: expected ~S, got ~S"
          pattern
          text
          expected
          actual))))))

(it
 "validates regex-set parallel compilation boundaries"
 (dolist (value '(0 9))
   (let ((cl-regex-kit::*nfa-merge-parallelism-override* value))
     (signals type-error (cl-regex-kit::nfa-merge-parallelism 2 65536)))
   (let ((cl-regex-kit::*regex-set-compile-parallelism-override* value))
     (signals type-error (cl-regex-kit::regex-set-compile-parallelism 2 65536))))
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
     (error "Size-limit rejection compiled ~D member patterns" calls)))
 (signals
  error
  (cl-regex-kit::compile-regex-set-chunk (vector "a") 0 1 #'compile-regex nil 2))
 (signals
  type-error
  (cl-regex-kit::compile-regex-set-chunk (vector "a") 0 1 #'compile-regex nil 9)))

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
       (expect (regex-set-matches set text) :to-equal (quote (3 7 15 16)))))))
(it
 "matches CRLF as one line break in regex-set execution"
 (let* ((text (format nil "~C~C" #\Return #\Newline))
        (set (compile-regex-set (list "\\R" "a")))
        (byte-text
         (make-array 2
                     :element-type (quote (unsigned-byte 8))
                     :initial-contents (list #x0d #x0a)))
        (byte-set (compile-byte-regex-set (list "\\R" "a"))))
   (expect (regex-set-matches set text) :to-equal (list 0))
   (expect (regex-set-matches byte-set byte-text) :to-equal (list 0))))
