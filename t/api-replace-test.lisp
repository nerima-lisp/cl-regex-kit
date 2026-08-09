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

(it
 "applies dollar templates on every replacement entry point"
 (let ((regex (compile-regex "(a)")))
   (expect (replace-first regex "aa" "[$1]") :to-equal "[a]a")
   (expect (replace-n regex "aa" "[$1]" 1) :to-equal "[a]a")
   (expect (replace-all regex "aa" "[$1]") :to-equal "[a][a]")))

(it
 "accepts functional replacements without a template parser"
 (let ((regex (compile-regex "(a)")))
   (expect
    (replace-all
     regex
     "a"
     (lambda (result text)
       (match-group-string result 1 text)))
    :to-equal
    "a")))

;;; Octet templates mirror the character-domain dollar syntax.
(it
 "expands byte dollar templates and leaves backslashes literal"
 (flet ((octets (&rest values)
          (make-array
           (length values)
           :element-type
           (quote (unsigned-byte 8))
           :initial-contents
           values)))
   (let ((regex (compile-byte-regex "(?<w>A)(B)")))
     (expect
      (coerce
       (replace-all regex (octets 65 66) (octets #x24 #x31))
       (quote list))
      :to-equal
      (quote (65)))
     (expect
      (coerce
       (replace-all regex (octets 65 66) (octets #x24 #x7b #x77 #x7d))
       (quote list))
      :to-equal
      (quote (65)))
     (expect
      (coerce
       (replace-all regex (octets 65 66) (octets #x5c #x31))
       (quote list))
      :to-equal
      (quote (92 49)))
     (expect
      (coerce
       (replace-all regex (octets 65 66) (octets #x5c #x26))
       (quote list))
      :to-equal
      (quote (92 38))))))

(it
 "expands numeric captures in byte replacements"
 (flet ((octets (&rest values)
          (make-array
           (length values)
           :element-type
           '(unsigned-byte 8)
           :initial-contents
           values)))
   (let ((result
          (replace-first
           (compile-byte-regex "(A)(B)?")
           (octets 65 66)
           (octets 36 48 45 36 49 45 36 50))))
     (unless (equalp result (octets 65 66 45 65 45 66))
       (error "Unexpected numeric byte replacement: ~S" (coerce result 'list))))))

(it
 "supports functional replacements for character and byte input"
 (flet ((octets (&rest values)
          (make-array
           (length values)
           :element-type
           '(unsigned-byte 8)
           :initial-contents
           values)))
   (expect
    (replace-all
     (compile-regex "(a)")
     "aba"
     (lambda (result text)
       (declare (ignore result text))
       "X"))
    :to-equal
    "XbX")
   (let ((result
          (replace-all
           (compile-byte-regex "(A)")
           (octets 65 66 65)
           (lambda (result text)
             (declare (ignore result text))
             (octets 88)))))
     (unless (equalp result (octets 88 66 88))
       (error
        "Unexpected functional byte replacement: ~S"
        (coerce result 'list))))))

(it
 "expands numeric and optional captures in replacements"
 (let ((regex (compile-regex "(a)(b)?")))
   (expect (replace-first regex "ab" "$0/$1/$2/$12") :to-equal "ab/a/b/")
   (expect (replace-first regex "a" "$0-$1-$2-$3") :to-equal "a-a--")))
