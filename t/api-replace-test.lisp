;;;; t/api-replace-test.lisp
;;;;
;;;; Dollar-style replacement template coverage. Every replacement caller uses
;;;; this sole public syntax, so a regression is a silent data-corruption bug.
(in-package #:cl-regex-kit/test)

(it-each
 ((("(?<word>a)") "a" "$$${word}-$missing-${2}-$" "$a---$")
  (("(?<word>a)") "a" "$" "$")
  (("(?<word>a)") "a" "$wordx" "")
  (("(?<word>a)") "a" "${}" "")
  (("(?<word>a)") "a" "${word" "${word")
  (("(?<word>a)") "a" "$-" "$-")
  (("(?<a.b>x)") "x" "$a.b" ".b")
  (("(?<a.b>x)") "x" "${a.b}" "x")
  (("(a)") "a" "\\1" "\\1")
  (("(a)") "a" "\\&" "\\&")
  (("(a)") "a" "\\0" "\\0")
  (("(a)") "a" "\\\\" "\\\\"))
 "expands dollar template ~S over ~S to ~S"
 (options text template expected)
(expect
  (replace-all (apply (function compile-regex) options) text template)
  :to-equal
  expected))

(it-replace-cases
 "applies dollar templates on every replacement entry point"
 #'compile-regex
 "(a)"
 (replace-first "aa" "[$1]" "[a]a")
 (replace-n "aa" "[$1]" "[a]a" 1)
 (replace-all "aa" "[$1]" "[a][a]"))

(it-replace-cases
 "accepts functional replacements without a template parser"
 #'compile-regex
 "(a)"
 (replace-all
  "a"
  (lambda (result text)
    (match-group-string result 1 text))
  "a"))

;;; Octet templates mirror the character-domain dollar syntax.
(it-byte-replace-cases
 "expands byte dollar templates and leaves backslashes literal"
 #'compile-byte-regex
 "(?<w>A)(B)"
 (replace-all (octets 65 66) (octets #x24 #x31) (quote (65)))
 (replace-all (octets 65 66) (octets #x24 #x7b #x77 #x7d) (quote (65)))
 (replace-all
  (octets 65 66)
  (octets #x24 #x24 #x24 #x7b #x77 #x7d #x2d #x24 #x6d #x69 #x73 #x73 #x69 #x6e
          #x67 #x2d #x24 #x7b #x32 #x7d #x2d #x24)
  (quote (36 65 45 45 66 45 36)))
 (replace-all (octets 65 66) (octets #x24) (quote (36)))
 (replace-all (octets 65 66) (octets #x24 #x77 #x6f #x72 #x64 #x78) (quote ()))
 (replace-all (octets 65 66) (octets #x24 #x7b #x7d) (quote ()))
 (replace-all
  (octets 65 66)
  (octets #x24 #x7b #x77 #x7d)
  (quote (65)))
 (replace-all
  (octets 65 66)
  (octets #x24 #x7b #x77)
  (quote (36 123 119)))
 (replace-all (octets 65 66) (octets #x24 #x2d) (quote (36 45)))
 (replace-all (octets 65 66) (octets #x5c #x31) (quote (92 49)))
 (replace-all (octets 65 66) (octets #x5c #x26) (quote (92 38))))

(it-byte-replace-cases
 "expands numeric captures in byte replacements"
 #'compile-byte-regex
 "(A)(B)?"
 (replace-first
  (octets 65 66)
  (octets 36 48 45 36 49 45 36 50)
  (quote (65 66 45 65 45 66))))

(it-replace-cases
 "supports functional replacements for character input"
 #'compile-regex
 "(a)"
 (replace-all
  "aba"
  (lambda (result text)
    (declare (ignore result text))
    "X")
  "XbX"))

(it-byte-replace-cases
 "supports functional replacements for byte input"
 #'compile-byte-regex
 "(A)"
 (replace-all
  (octets 65 66 65)
  (lambda (result text)
    (declare (ignore result text))
    (octets 88))
  (quote (88 66 88))))

(it-replace-cases
 "expands numeric and optional captures in replacements"
 #'compile-regex
 "(a)(b)?"
 (replace-first "ab" "$0/$1/$2/$12" "ab/a/b/")
 (replace-first "a" "$0-$1-$2-$3" "a-a--"))
