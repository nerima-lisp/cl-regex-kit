;;;; t/parser-test.lisp
(in-package #:cl-regex-kit/test)

(it "parses alternation, groups, repetitions, classes, and anchors"
  (let ((ast (cl-regex-kit::parse-regex "^(ab|c+?)[a-z]$")))
    (expect (typep ast 'cl-regex-kit::concat-node) :to-be-truthy)
    (expect (length (cl-regex-kit::concat-node-children ast)) :to-equal 4)))

(it "rejects a non-string pattern with regex-syntax-error"
  (signals regex-syntax-error (cl-regex-kit::parse-regex nil)))

(it-each (("(abc") ("a{3,2}") ("[z-a]") ("\\") ("\\q") ("[\\q]")
          ("\\p{Definitely_Not_A_Property}") ("\\p{Script=Definitely_Not_A_Script}")
          ("\\p{}") ("\\p{") ("\\p") ("\\x") ("\\x{}") ("\\x{1")
          ("\\x{110000}") ("\\x{D800}") ("\\uD800") ("\\u{D800}") ("\\U0000D800")
          ("\\U{D800}") ("\\u123") ("\\U00110000") ("\\400")
          ("\\o") ("\\o{}") ("\\o{8}") ("\\o{1") ("\\o{400000000}")
          ("\\b{") ("\\b{middle}") ("\\b{start") ("[[:]]") ("[[:unknown:]]")
          ("(?[a])") ("(?[[a]&])") ("(?[[a]~[b]])")
          ("[a-\\d]") ("[a") ("[") ("[a&&]") ("(?Pname)") ("(?q)a")
          ("(?i?a)") ("(?ii)a") ("(?i-i)a") ("(?i--m)a")
          ("a{") ("a{1") ("a{1,2") ("a{1,0}") ("a{1001}") ("a**") ("("))
    "rejects the malformed pattern ~S with regex-syntax-error"
    (pattern)
  (signals regex-syntax-error (cl-regex-kit::parse-regex pattern)))

(it "accepts Rust Unicode Age aliases"
  (expect (cl-regex-kit::parse-regex "\\p{Age=15.1}") :to-be-truthy)
  (expect (cl-regex-kit::parse-regex "\\p{Age=V15_1}") :to-be-truthy)
  (expect (cl-regex-kit::parse-regex "\\p{Age=v151}") :to-be-truthy))

(defun escape-form-text (designator)
  "Translate a symbolic ESCAPE-FORM-TEST row designator to its literal text."
  (case designator
    (:tab (string #\Tab))
    (:return (string #\Return))
    (:vertical-tab (string (code-char 11)))
    (:nul (string (code-char 0)))
    (:start-of-heading (string (code-char 1)))
    (:line-feed (string #\Newline))
    (otherwise designator)))

(it-each (("\\d" "5" t) ("\\D" "x" t) ("\\s" :tab t) ("\\P{ASCII}" "é" t)
          ("\\x{41}" "A" t) ("\\u0041" "A" t) ("\\U00000041" "A" t)
          ("\\o{101}" "A" t) ("(?x)\\o{ 1 0 1 }" "A" t)
          ("\\o{141}" "a" nil)
          ("\\0" :nul t) ("\\00" :nul t) ("\\1" :start-of-heading t)
          ("\\12" :line-feed t) ("\\141" "a" t) ("\\r" :return t)
          ("\\v" :vertical-tab t) ("\\d" "5" nil) ("\\s" :tab nil))
    "matches ~S against ~S under RE2/Rust semantics (unicode ~A)"
    (pattern text unicode)
  (expect (escape-form-text text) :to-match-regex (compile-regex pattern :unicode unicode)))

(it-each (("[\\0]" #.(code-char 0)) ("[\\12]" #\Newline) ("[\\141]" #\a)
          ("[\\o{141}]" #\a) ("(?x)[\\o{ 1 4 1 }]" #\a))
    "accepts braced and legacy octal escapes inside a character class"
    (pattern character)
  (expect (string character) :to-match-regex (compile-regex pattern)))

(it-each (("alnum" #\A #\-) ("alpha" #\A #\1) ("ascii" #.(code-char 0) #.(code-char #x80))
          ("blank" #\Tab #\Newline) ("cntrl" #.(code-char 0) #\A) ("digit" #\1 #\A)
          ("graph" #\! #\Space) ("lower" #\a #\A) ("print" #\Space #.(code-char 127))
          ("punct" #\! #\A) ("space" #\Newline #\A) ("upper" #\A #\a)
          ("word" #\_ #\-) ("xdigit" #\F #\G))
    "supports the RE2 ASCII POSIX class [[:~A:]], matching ~S but not ~S"
    (name member non-member)
  (let ((regex (compile-regex (format nil "[[:~A:]]" name))))
    (expect (full-match regex (string member)) :to-be-truthy)
    (expect (full-match regex (string non-member)) :to-be-falsy)))

(it "negates a POSIX character class with the inner [:^name:] form"
  (let ((regex (compile-regex "[[:^digit:]]")))
    (expect (full-match regex "A") :to-be-truthy)
    (expect (full-match regex "1") :to-be-falsy)))

(it "parses RE2 and Rust-style flags and named captures"
  (expect (typep (cl-regex-kit::parse-regex "(?im-s:foo)(?<name>bar)(?P<id>\\d+)")
                 'cl-regex-kit::concat-node)
          :to-be-truthy)
  (expect (typep (cl-regex-kit::parse-regex "(?<part_2>x)")
                 'cl-regex-kit::group-node) :to-be-truthy)
  (dolist (pattern '("(?<part.name>x)" "(?<part[2]>x)" "(?<Δ>x)"))
    (expect (typep (cl-regex-kit::parse-regex pattern)
                   'cl-regex-kit::group-node) :to-be-truthy))
  (expect (typep (cl-regex-kit::parse-regex
                  (format nil "(?<~Ca~C~C>x)"
                          (code-char #x2160) ; Alphabetic, Nl
                          (code-char #x00b2) ; No
                          (code-char #x0661))) ; Nd
                 'cl-regex-kit::group-node)
          :to-be-truthy)
  (signals regex-syntax-error
    (cl-regex-kit::parse-regex (format nil "(?<a~Cb>x)" (code-char #x203f))))
  (dolist (pattern '("(?<2part>x)" "(?<.part>x)" "(?<[part>x)"))
    (signals regex-syntax-error (cl-regex-kit::parse-regex pattern)))
  (signals regex-syntax-error (cl-regex-kit::parse-regex "(?<name>a)(?<name>b)"))
  (expect (typep (cl-regex-kit::parse-regex "(?=a)") (quote cl-regex-kit::lookaround-node)) :to-be-truthy))

(it "treats backslash-b inside a character class as backspace"
  (let ((regex (compile-regex "[\\b]")))
    (expect (string (code-char 8)) :to-match-regex regex)
    (expect-not "b" :to-match-regex regex)))

(it "accepts empty character classes as empty sets"
  (let ((regex (compile-regex "[]")))
    (expect-not "" :to-match-regex regex)
    (expect-not "a" :to-match-regex regex))
  (let ((regex (compile-regex "[^]")))
    (expect-not "" :to-match-regex regex)
    (expect "a" :to-match-regex regex))
  (let ((empty (make-array 0 :element-type '(unsigned-byte 8)))
        (a (make-array 1 :element-type '(unsigned-byte 8)
                          :initial-contents '(97))))
    (let ((regex (compile-byte-regex "[]")))
      (expect-not empty :to-match-regex regex)
      (expect-not a :to-match-regex regex))
    (let ((regex (compile-byte-regex "[^]")))
      (expect-not empty :to-match-regex regex)
      (expect a :to-match-regex regex)))
  (let ((regex (compile-regex "[[a]&&[]]")))
    (expect-not "a" :to-match-regex regex)))

(it "supports Perl extended character classes and set algebra"
  (dolist (row '(("(?[[a-c]+[x-z]])" ("a" "x") ("d" "w"))
                 ("(?[[a-z]&[d-f]])" ("d" "f") ("a" "x"))
                 ("(?[[a-c]-[b-d]])" ("a") ("b" "d"))
                 ("(?[[a-c]^[b-d]])" ("a" "d") ("b" "c"))
                 ("(?[[a-c]&&[b-c]])" ("b" "c") ("a" "x"))
                 ("(?[[a-c]~~[b-c]])" ("a") ("b" "x"))
                 ("(?[[a-c]--[b-c]])" ("a") ("b" "x"))
                 ("(?[[a-c]||[x]])" ("a" "c" "x") ("d" "w"))
                 ("(?[![a]])" ("b") ("a"))))
    (destructuring-bind (pattern members non-members) row
      (let ((regex (compile-regex pattern)))
        (dolist (member members)
          (expect (full-match regex member) :to-be-truthy))
        (dolist (non-member non-members)
          (expect (full-match regex non-member) :to-be-falsy)))))
  (let ((regex (compile-regex "(?[([a-c]&[b-d])|[x]])")))
    (expect (full-match regex "b") :to-be-truthy)
    (expect (full-match regex "x") :to-be-truthy)
    (expect (full-match regex "a") :to-be-falsy))
  (let ((regex (compile-regex "(?[\\d+\\p{Lu}])")))
    (expect (full-match regex "5") :to-be-truthy)
    (expect (full-match regex "A") :to-be-truthy)
    (expect (full-match regex "x") :to-be-falsy))
  (let ((regex (compile-regex "(?[[:digit:]])")))
    (expect (full-match regex "5") :to-be-truthy)
    (expect (full-match regex "x") :to-be-falsy)))

(it "handles empty and escaped atoms in extended character classes"
  (let ((regex (compile-regex "(?[])")))
    (expect-not "" :to-match-regex regex)
    (expect-not "a" :to-match-regex regex))
  (let ((regex (compile-regex "(?[\\x{41}])")))
    (expect "A" :to-match-regex regex)
    (expect-not "B" :to-match-regex regex))
  (dolist (pattern '("(?[(a])" "(?[[a]"))
    (signals regex-syntax-error (cl-regex-kit::parse-regex pattern))))

(it "flushes accumulated literal ranges before a later class-item kind"
  (let ((regex (compile-regex "[a[:digit:]]")))
    (expect "a" :to-match-regex regex)
    (expect "5" :to-match-regex regex)
    (expect-not "b" :to-match-regex regex))
  (let ((regex (compile-regex "[a\\d]")))
    (expect "a" :to-match-regex regex)
    (expect "5" :to-match-regex regex)
    (expect-not "b" :to-match-regex regex)))

(it "negates a Unicode property escape inside a character class"
  (let ((regex (compile-regex "[\\P{Lu}]")))
    (expect "a" :to-match-regex regex)
    (expect-not "A" :to-match-regex regex)))

(it "treats non-boundary escapes as literals inside a character class"
  (dolist (row '(("[\\A]" "A") ("[\\B]" "B") ("[\\C]" "C")
                 ("[\\G]" "G") ("[\\K]" "K") ("[\\Q]" "Q")
                 ("[\\Z]" "Z") ("[\\z]" "z") ("[\\<]" "<")
                 ("[\\>]" ">")))
    (destructuring-bind (pattern character) row
      (expect character :to-match-regex (compile-regex pattern))))
  (dolist (pattern '("[\\R]" "[\\X]" "[\\g<1>]" "[\\k<name>]" "[\\8]"))
    (signals regex-syntax-error (cl-regex-kit::parse-regex pattern)))
  (signals regex-syntax-error
    (cl-regex-kit::parse-regex "[\\0]" :octal nil)))

(it "covers byte shorthand classes and parser option contracts"
  (dolist (row '(("[\\d]" 53) ("[\\D]" 65) ("[\\w]" 65)
                 ("[\\W]" 45) ("[\\s]" 32) ("[\\S]" 65)
                 ("[\\h]" 9) ("[\\H]" 65)))
    (destructuring-bind (pattern code) row
      (let ((input (make-array 1 :element-type '(unsigned-byte 8)
                               :initial-contents (list code))))
        (expect input :to-match-regex (compile-byte-regex pattern)))))
  (expect (cl-regex-kit::parse-regex "a.b" :literal t) :to-be-truthy)
  (expect (cl-regex-kit::parse-regex "(a)" :never-capture t) :to-be-truthy)
  (expect (cl-regex-kit::parse-regex "\\N" :line-terminator #\Return)
          :to-be-truthy)
  (expect (cl-regex-kit::parse-regex "a" :nest-limit 2) :to-be-truthy)
  (expect (cl-regex-kit::union-matcher nil) :to-equal '(:ranges nil))
  (dolist (arguments '((:initial-flags nil) (:byte-mode 1) (:literal 1)
                       (:never-capture 1) (:octal 1) (:nest-limit -1)
                       (:line-terminator "x")))
    (signals type-error (apply #'cl-regex-kit::parse-regex "a" arguments))))

(it "rejects incomplete comments and malformed scanner targets"
  (dolist (pattern '("(?#unclosed" "a??+"))
    (signals regex-syntax-error (cl-regex-kit::parse-regex pattern)))
  (dolist (case '(("\\N{" 3)
                  ("\\N{}" 3)))
    (destructuring-bind (pattern position) case
      (signals regex-syntax-error
        (cl-regex-kit::scan-named-character pattern position))))
  (dolist (case '(("\\g" #\g) ("\\g<1" #\g) ("\\g<>" #\g)
                  ("\\g{1a}" #\g) ("\\g{a-}" #\g)
                  ("\\k{+1}" #\k) ("\\g{+0}" #\g)
                  ("\\g<1a>" #\g) ("\\g<name->" #\g)))
    (destructuring-bind (pattern kind) case
      (signals regex-syntax-error
        (cl-regex-kit::scan-backreference pattern 2 kind))))
  (dolist (pattern '("(?[" "(?[(\\d])" "(?[[a]]"))
    (signals regex-syntax-error (cl-regex-kit::parse-regex pattern))))

(it "matches a lowercase byte against an uppercase-only case-insensitive class"
  (let ((regex (compile-byte-regex "(?i-u:[A-Z])")))
    (expect (make-array 1 :element-type '(unsigned-byte 8) :initial-contents '(#x61))
            :to-match-regex regex)))

(it "prefers the empty match for a lazy optional quantifier"
  (expect (match-string (match "a??" "a") "a") :to-equal ""))

(it "validates capture-name characters and parser flags"
  (expect (cl-regex-kit::capture-name-start-p #\a) :to-be-truthy)
  (expect (cl-regex-kit::capture-name-start-p #\_) :to-be-truthy)
  (expect (cl-regex-kit::capture-name-start-p #\1) :to-be-falsy)
  (expect (cl-regex-kit::capture-name-character-p #\1) :to-be-truthy)
  (expect (cl-regex-kit::capture-name-character-p (code-char #x00b2)) :to-be-truthy)
  (expect (cl-regex-kit::capture-name-character-p #\.) :to-be-truthy)
  (expect (cl-regex-kit::capture-name-character-p #\-) :to-be-falsy)
  (expect (cl-regex-kit::id-start-p #\A) :to-be-truthy)
  (expect (cl-regex-kit::id-continue-p #\1) :to-be-truthy)
  (expect (logtest cl-regex-kit::+flag-case-insensitive+
                   (cl-regex-kit::make-parser-flags :case-insensitive t))
          :to-be-truthy)
  (expect (logtest cl-regex-kit::+flag-crlf+
                   (cl-regex-kit::make-parser-flags :crlf t))
          :to-be-truthy)
  (signals error
    (cl-regex-kit::required-unicode-runtime-property-values
      :category (quote ("DEFINITELYUNKNOWNCATEGORY")))))

(it "maps every compilation option to an independent parser flag"
  (let ((flags (cl-regex-kit::make-parser-flags
                :case-insensitive t :multi-line t :dot-matches-new-line t
                :swap-greed t :ignore-whitespace t :unicode t :crlf t)))
    (dolist (flag (list cl-regex-kit::+flag-case-insensitive+
                        cl-regex-kit::+flag-multiline+
                        cl-regex-kit::+flag-dotall+
                        cl-regex-kit::+flag-ungreedy+
                        cl-regex-kit::+flag-extended+
                        cl-regex-kit::+flag-unicode+
                        cl-regex-kit::+flag-crlf+))
      (expect (logtest flag flags) :to-be-truthy))
    (expect (logtest cl-regex-kit::+flag-unicode+
                     (cl-regex-kit::make-parser-flags))
            :to-be-truthy)
    (expect (logtest cl-regex-kit::+flag-unicode+
                     (cl-regex-kit::make-parser-flags :unicode nil))
            :to-be-falsy))
  (dolist (arguments '((:case-insensitive 1)
                       (:multi-line 1)
                       (:dot-matches-new-line 1)
                       (:swap-greed 1)
                       (:ignore-whitespace 1)
                       (:unicode 1)
                       (:crlf 1)))
    (signals type-error (apply #'cl-regex-kit::make-parser-flags arguments))))

(it "updates inline flags independently and restores parser nesting depth"
  (let ((flags 0))
    (dolist (entry `((#\i ,cl-regex-kit::+flag-case-insensitive+)
                     (#\m ,cl-regex-kit::+flag-multiline+)
                     (#\s ,cl-regex-kit::+flag-dotall+)
                     (#\U ,cl-regex-kit::+flag-ungreedy+)
                     (#\x ,cl-regex-kit::+flag-extended+)
                     (#\u ,cl-regex-kit::+flag-unicode+)
                     (#\R ,cl-regex-kit::+flag-crlf+)
                     (#\n ,cl-regex-kit::+flag-no-auto-capture+)
                     (#\J ,cl-regex-kit::+flag-duplicate-names+)))
      (destructuring-bind (character flag) entry
        (setf flags (cl-regex-kit::update-parser-flag flags character t))
        (expect (logtest flag flags) :to-be-truthy)
        (setf flags (cl-regex-kit::update-parser-flag flags character nil))
        (expect (logtest flag flags) :to-be-falsy))))
  (let ((depth 0))
    (expect (cl-regex-kit::with-parser-nesting (depth 1 (error "overflow"))
              depth)
            :to-equal 1)
    (expect depth :to-equal 0)
    (signals simple-error
      (cl-regex-kit::with-parser-nesting (depth 0 (error "overflow"))
        :unreachable))
    (expect depth :to-equal 0)))

(it-fuzz
  "arbitrary bounded byte-mode patterns either parse or report a syntax error"
  ((pattern (gen-string :min-length 0
                       :max-length 80
                       :alphabet "aAzZ09()[]{}?*+|\\\\.^$:<>,#_- \t\n")))
  (:trials 200 :timeout-per-trial 1)
  (handler-case
      (cl-regex-kit::parse-regex pattern :byte-mode t :initial-flags 0)
    (regex-syntax-error () nil)))

(it "rejects Unicode-only syntax in non-Unicode byte patterns"
  (signals regex-syntax-error
    (cl-regex-kit::parse-regex "\\p{L}" :byte-mode t :initial-flags 0))
  (signals regex-syntax-error
    (cl-regex-kit::parse-regex "\\p{L"))
  (signals regex-syntax-error
    (cl-regex-kit::parse-regex "\\C"))
  (expect (typep (cl-regex-kit::parse-regex "\\C" :byte-mode t)
                 'cl-regex-kit::any-char-node)
          :to-be-truthy))

(it "rejects non-ASCII and Unicode-property class content in non-Unicode byte patterns"
  (dolist (pattern (quote ("[\\p{L}]" "[\\x{e9}]")))
    (signals regex-syntax-error
      (cl-regex-kit::parse-regex pattern :byte-mode t :initial-flags 0)))
  ;; A raw single-octet escape is exempt: it explicitly names one byte value,
  ;; not a Unicode scalar the parser would otherwise have to reject.
  (expect (cl-regex-kit::parse-regex "[\\xe9]" :byte-mode t :initial-flags 0)
          :to-be-truthy)
  (expect (cl-regex-kit::parse-regex "[\\o{351}]" :byte-mode t :initial-flags 0)
          :to-be-truthy)
  (signals regex-syntax-error
    (cl-regex-kit::parse-regex "[\\o{400}]" :byte-mode t :initial-flags 0)))
(it "supports Unicode whitespace, non-newline, named characters, and line breaks"
  (let* ((horizontal (compile-regex "\\h+"))
         (not-horizontal (compile-regex "\\H+"))
         (not-newline (compile-regex "\\N+"))
         (not-newline-crlf (compile-regex "[\\N]+" :crlf t))
         (line-break (compile-regex "\\R"))
         (named (compile-regex "\\N{LATIN CAPITAL LETTER A}"))
         (named-control (compile-regex "\\N{LINE FEED}"))
         (crlf (format nil "~C~C" #\Return #\Newline))
         (crlf-result (scan line-break crlf))
         (byte-text (make-array 2
                                :element-type (quote (unsigned-byte 8))
                                :initial-contents (quote (#x0d #x0a))))
         (byte-result (scan (compile-byte-regex "\\R") byte-text)))
    (expect (full-match horizontal (format nil "~C~C" #\Tab #\Space))
            :to-be-truthy)
    (expect (full-match horizontal (string #\Newline)) :to-be-falsy)
    (expect (full-match horizontal (string (code-char #xa0)))
            :to-be-truthy)
    (expect (full-match horizontal (string (code-char #x180e)))
            :to-be-truthy)
    (expect (full-match not-horizontal "a") :to-be-truthy)
    (expect (full-match not-horizontal (string #\Space)) :to-be-falsy)
    (expect (full-match not-newline "abc") :to-be-truthy)
    (expect (full-match not-newline (format nil "a~Cb" #\Newline))
            :to-be-falsy)
    (expect (full-match not-newline-crlf "a") :to-be-truthy)
    (expect (full-match not-newline-crlf (string #\Return)) :to-be-falsy)
    (expect (full-match not-newline-crlf (string #\Newline)) :to-be-falsy)
    (expect (full-match named "A") :to-be-truthy)
    (expect (full-match named-control (string #\Newline)) :to-be-truthy)
    (expect (full-match line-break crlf) :to-be-truthy)
    (expect (match-end crlf-result) :to-equal 2)
    (expect (full-match line-break (string #\Newline)) :to-be-truthy)
    (expect (full-match line-break (string (code-char #x2028)))
            :to-be-truthy)
    (expect (full-match line-break (string (code-char #x2029)))
            :to-be-truthy)
    (expect (full-match line-break "x") :to-be-falsy)
    (expect (match-end byte-result) :to-equal 2)
    (signals regex-syntax-error
      (compile-regex "\\N{NOT_A_REAL_CHARACTER}"))))

(it "runs an unterminated quoted literal to the end of the pattern"
  (let ((regex (compile-regex "\\Qa.b")))
    (expect (full-match regex "a.b") :to-be-truthy)
    (expect (full-match regex "aXb") :to-be-falsy)))

(it "rejects an unclosed named-capture body and an unnamed group's missing < after ?P"
  (dolist (pattern '("(?P<name" "(?<name"))
    (signals regex-syntax-error (cl-regex-kit::parse-regex pattern))))
(it "reports unclosed numeric subroutine references as syntax errors"
  (dolist (pattern '("(?1" "(?-1"))
    (signals regex-syntax-error (cl-regex-kit::parse-regex pattern))))
(it "rejects malformed advanced-group forms"
  (dolist (pattern
           '("(*PLA)"
             "(*PLA:a"
             "(*SKIP:)"
             "(?>a"
             "(?+1)"
             "(?-1)"
             "(?()"
             "(?(1)a"
             "(?(R1a)"
             "(?(R?)a)"
             "(?(<name>a)"))
    (signals regex-syntax-error (cl-regex-kit::parse-regex pattern))))
(it "rejects constructs outside the current regex dialect" (dolist (pattern (quote ("(?Cx)" "(?C1x)" "(?C\"tag)" "(?{1})" "(??{1})" "a{~1}" "(*UNKNOWN)" "(?&1)" "(?&name" "(?&name]" "(*FAIL:tag)" "(*MARK:)" "(*MARK:(?))" "(*COMMIT" "(?=a" "(?*a" "(?<*a" "(?|(a)"))) (signals regex-syntax-error (cl-regex-kit::parse-regex pattern))))
(it "parses PCRE2-style callouts"
  (let ((plain (cl-regex-kit::parse-regex "(?C)"))
        (numbered (cl-regex-kit::parse-regex "(?C42)"))
        (tagged (cl-regex-kit::parse-regex "(?C\"mark\")")))
    (expect (typep plain (quote cl-regex-kit::callout-node)) :to-be-truthy)
    (expect (cl-regex-kit::callout-node-number numbered) :to-equal 42)
    (expect (cl-regex-kit::callout-node-tag tagged) :to-equal "mark")
    (dolist (pattern
             (list "(?C\"mark\")"
                   (format nil "(?C~Cmark~C)" (code-char 39) (code-char 39))
                   "(?C^mark^)"
                   "(?C%mark%)"
                   "(?C#mark#)"
                   "(?C$mark$)"
                   "(?C{mark})"))
      (expect (cl-regex-kit::callout-node-tag
              (cl-regex-kit::parse-regex pattern))
              :to-equal "mark"))))

(it "returns stable values for tokenizer escape forms"
  (multiple-value-bind (character raw-octet-p next-position)
      (cl-regex-kit::scan-escaped-character "\\x41tail" 1)
    (expect character :to-equal #\A)
    (expect raw-octet-p :to-be-truthy)
    (expect next-position :to-equal 4))
  (multiple-value-bind (character raw-octet-p next-position)
      (cl-regex-kit::scan-escaped-character "\\U0001F600" 1)
    (expect character :to-equal (code-char #x1f600))
    (expect raw-octet-p :to-be-null)
    (expect next-position :to-equal 10))
  (multiple-value-bind (character next-position)
      (cl-regex-kit::scan-octal-code "141z" 1)
    (expect character :to-equal #\a)
    (expect next-position :to-equal 3))
  (multiple-value-bind (digits next-position)
      (cl-regex-kit::scan-while "x41" 0 #'cl-regex-kit::hex-digit-p)
    (expect digits :to-equal "")
    (expect next-position :to-equal 0)))

(it "normalizes Unicode property and quoted-literal scanner forms"
  (dolist (case (quote (("\\p{gc!=Lu}" "gc=Lu" t)
                         ("\\p{Script:Greek}" "Script=Greek" nil)
                         ("\\pL" "L" nil))))
    (destructuring-bind (pattern expected-name expected-negated-p) case
      (multiple-value-bind (descriptor negated-p next-position)
          (cl-regex-kit::scan-unicode-property-name pattern 2)
        (let ((expected-descriptor
                (cl-regex-kit::resolve-unicode-property expected-name)))
          (expect
           (and (eq (cl-regex-kit::unicode-property-descriptor-kind descriptor)
                    (cl-regex-kit::unicode-property-descriptor-kind
                     expected-descriptor))
                (equalp (cl-regex-kit::unicode-property-descriptor-payload descriptor)
                        (cl-regex-kit::unicode-property-descriptor-payload
                         expected-descriptor)))
           :to-be-truthy))
        (expect negated-p :to-equal expected-negated-p)
        (expect next-position :to-equal (length pattern)))))
  (multiple-value-bind (text next-position)
      (cl-regex-kit::scan-quoted-literal "\\Qa.b\\Eb" 2)
    (expect text :to-equal "a.b")
    (expect next-position :to-equal 7))
  (multiple-value-bind (text next-position)
      (cl-regex-kit::scan-quoted-literal "\\Qunterminated" 2)
    (expect text :to-equal "unterminated")
    (expect next-position :to-equal (length "\\Qunterminated"))))

(it "decodes named characters and backreference targets"
  (multiple-value-bind (character next-position)
      (cl-regex-kit::scan-named-character "\\N{LINE FEED}x" 3)
    (expect character :to-equal #\Newline)
    (expect next-position :to-equal 13))
  (multiple-value-bind (capture-index name next-position relative-index subroutine-p)
      (cl-regex-kit::scan-backreference "\\g<12>" 2 #\g)
    (expect capture-index :to-equal 12)
    (expect name :to-be-null)
    (expect next-position :to-equal 6)
    (expect relative-index :to-be-null)
    (expect subroutine-p :to-be-null))
  (multiple-value-bind (capture-index name next-position relative-index subroutine-p)
      (cl-regex-kit::scan-backreference "\\g{+2}" 2 #\g)
    (expect capture-index :to-be-null)
    (expect name :to-be-null)
    (expect next-position :to-equal 6)
    (expect relative-index :to-equal 2)
    (expect subroutine-p :to-be-null))
  (multiple-value-bind (capture-index name next-position relative-index subroutine-p)
      (cl-regex-kit::scan-backreference "\\k{name}" 2 #\k)
    (expect capture-index :to-be-null)
    (expect name :to-equal "name")
    (expect next-position :to-equal 8)
    (expect relative-index :to-be-null)
    (expect subroutine-p :to-be-null))
  (multiple-value-bind (capture-index name next-position relative-index subroutine-p)
      (cl-regex-kit::scan-backreference "\\g'1'" 2 #\g)
    (expect capture-index :to-equal 1)
    (expect name :to-be-null)
    (expect next-position :to-equal 5)
    (expect relative-index :to-be-null)
    (expect subroutine-p :to-be-truthy))
  (signals regex-syntax-error
    (cl-regex-kit::scan-backreference "\\k<1>" 2 #\k))
  (signals regex-syntax-error
    (cl-regex-kit::scan-backreference "\\g{0}" 2 #\g)))
