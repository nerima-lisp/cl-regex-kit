;;;; t/parser-test.lisp
(in-package #:cl-regex-kit/test)

(it "parses alternation, groups, repetitions, classes, and anchors"
  (let ((ast (cl-regex-kit::parse-regex "^(ab|c+?)[a-z]$")))
    (expect (typep ast 'cl-regex-kit::concat-node) :to-be-truthy)
    (expect (length (cl-regex-kit::concat-node-children ast)) :to-equal 4)))

(it "rejects malformed patterns with regex-syntax-error"
  (signals regex-syntax-error (cl-regex-kit::parse-regex "(abc"))
  (signals regex-syntax-error (cl-regex-kit::parse-regex "a{3,2}"))
  (signals regex-syntax-error (cl-regex-kit::parse-regex "[z-a]"))
  (signals regex-syntax-error (cl-regex-kit::parse-regex "\\"))
  (signals regex-syntax-error (cl-regex-kit::parse-regex "\\q"))
  (signals regex-syntax-error (cl-regex-kit::parse-regex "[\\q]"))
  (signals regex-syntax-error
           (cl-regex-kit::parse-regex "\\p{Definitely_Not_A_Property}"))
    (signals regex-syntax-error
             (cl-regex-kit::parse-regex "\\p{Script=Definitely_Not_A_Script}")))

(it "accepts Rust Unicode Age aliases"
  (expect (cl-regex-kit::parse-regex "\\p{Age=15.1}") :to-be-truthy)
  (expect (cl-regex-kit::parse-regex "\\p{Age=V15_1}") :to-be-truthy)
  (expect (cl-regex-kit::parse-regex "\\p{Age=v151}") :to-be-truthy))

(it "rejects malformed escapes, classes, groups, and repetitions consistently"
  (signals regex-syntax-error (cl-regex-kit::parse-regex nil))
  (dolist (pattern '("\\p{}" "\\p{" "\\p" "\\x" "\\x{}" "\\x{1"
                     "\\x{110000}" "\\x{D800}" "\\uD800" "\\u{D800}" "\\U0000D800" "\\U{D800}" "\\u123" "\\U00110000" "\\400"
                     "\\b{" "\\b{middle}" "[[:]]" "[[:unknown:]]"
                     "[a-\\d]" "[a" "[a&&]" "(?Pname)" "(?q)a"
                     "(?i?a)" "(?ii)a" "(?i-i)a" "(?i--m)a"
                     "a{" "a{1,0}" "a{1001}" "a**"))
    (signals regex-syntax-error (cl-regex-kit::parse-regex pattern))))

(it "supports RE2 and Rust escape forms under Unicode and ASCII modes"
  (dolist (case '(("\\d" "5" t)
                  ("\\D" "x" t)
                  ("\\s" :tab t)
                  ("\\P{ASCII}" "é" t)
                  ("\\x{41}" "A" t)
                  ("\\u0041" "A" t)
                  ("\\U00000041" "A" t)
                  ("\\0" :nul t)
                  ("\\00" :nul t)
                  ("\\1" :start-of-heading t)
                  ("\\12" :line-feed t)
                  ("\\141" "a" t)
                  ("\\r" :return t)
                  ("\\v" :vertical-tab t)
                  ("\\d" "5" nil)
                  ("\\s" :tab nil)))
    (destructuring-bind (pattern text unicode) case
      (expect (is-match-p (compile-regex pattern :unicode unicode)
                          (case text
                            (:tab (string #\Tab))
                            (:return (string #\Return))
                            (:vertical-tab (string (code-char 11)))
                            (:nul (string (code-char 0)))
                            (:start-of-heading (string (code-char 1)))
                            (:line-feed (string #\Newline))
                            (otherwise text)))
              :to-be-truthy))))

(it "accepts one- through three-digit octal escapes in character classes"
  (dolist (case `(("[\\0]" ,(code-char 0))
                  ("[\\12]" ,(code-char 10))
                  ("[\\141]" #\a)))
    (destructuring-bind (pattern character) case
      (expect (is-match-p (compile-regex pattern) (string character))
              :to-be-truthy))))

(it "supports every RE2 ASCII POSIX character class and inner negation"
  (dolist (case `(("alnum" #\A #\-)
                  ("alpha" #\A #\1)
                  ("ascii" ,(code-char 0) ,(code-char #x80))
                  ("blank" #\Tab #\Newline)
                  ("cntrl" ,(code-char 0) #\A)
                  ("digit" #\1 #\A)
                  ("graph" #\! #\Space)
                  ("lower" #\a #\A)
                  ("print" #\Space ,(code-char 127))
                  ("punct" #\! #\A)
                  ("space" #\Newline #\A)
                  ("upper" #\A #\a)
                  ("word" #\_ #\-)
                  ("xdigit" #\F #\G)))
    (destructuring-bind (name member non-member) case
      (let ((regex (compile-regex (format nil "[[:~A:]]" name))))
        (expect (full-match regex (string member)) :to-be-truthy)
        (expect (full-match regex (string non-member)) :to-be-falsy))))
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
  (signals regex-syntax-error (cl-regex-kit::parse-regex "(?=a)")))

(it "treats backslash-b inside a character class as backspace"
  (let ((regex (compile-regex "[\\b]")))
    (expect (is-match-p regex (string (code-char 8))) :to-be-truthy)
    (expect (is-match-p regex "b") :to-be-falsy)))

(it "accepts empty character classes as empty sets"
  (let ((regex (compile-regex "[]")))
    (expect (is-match-p regex "") :to-be-falsy)
    (expect (is-match-p regex "a") :to-be-falsy))
  (let ((regex (compile-regex "[^]")))
    (expect (is-match-p regex "") :to-be-falsy)
    (expect (is-match-p regex "a") :to-be-truthy))
  (let ((empty (make-array 0 :element-type '(unsigned-byte 8)))
        (a (make-array 1 :element-type '(unsigned-byte 8)
                          :initial-contents '(97))))
    (let ((regex (compile-byte-regex "[]")))
      (expect (is-match-p regex empty) :to-be-falsy)
      (expect (is-match-p regex a) :to-be-falsy))
    (let ((regex (compile-byte-regex "[^]")))
      (expect (is-match-p regex empty) :to-be-falsy)
      (expect (is-match-p regex a) :to-be-truthy)))
  (let ((regex (compile-regex "[[a]&&[]]")))
    (expect (is-match-p regex "a") :to-be-falsy)))

(it "validates capture-name characters and initial parser options"
  (expect (cl-regex-kit::capture-name-start-p #\a) :to-be-truthy)
  (expect (cl-regex-kit::capture-name-start-p #\_) :to-be-truthy)
  (expect (cl-regex-kit::capture-name-start-p #\1) :to-be-falsy)
  (expect (cl-regex-kit::capture-name-character-p #\1) :to-be-truthy)
  (expect (cl-regex-kit::capture-name-character-p #\.) :to-be-truthy)
  (expect (cl-regex-kit::capture-name-character-p #\-) :to-be-falsy)
  (expect (logtest cl-regex-kit::+flag-case-insensitive+
                   (cl-regex-kit::make-parser-flags :case-insensitive t))
          :to-be-truthy)
  (expect (logtest cl-regex-kit::+flag-crlf+
                   (cl-regex-kit::make-parser-flags :crlf t))
          :to-be-truthy))

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
                     (#\R ,cl-regex-kit::+flag-crlf+)))
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
