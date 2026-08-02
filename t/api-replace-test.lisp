;;;; t/api-replace-test.lisp
;;;;
;;;; Replacement templates in both dialects. The `:dollar` rows exist to pin
;;;; the default: cl-cc-vm and every current caller pass no :TEMPLATE-SYNTAX,
;;;; so a regression there is a silent data-corruption bug, not a test failure
;;;; anyone would notice from the call site.
(in-package #:cl-regex-kit/test)

;;; Backslash syntax: the cl-ppcre-compatible dialect cl-tmux's
;;; `#{s/pattern/replacement/}` format modifier exposes to end users.

(it-each ((("(a)(b)")     "ab"   "\\2\\1"   "ba")
          (("(a)")        "a"    "\\1"      "a")
          (("(a)")        "a"    "\\1\\1"   "aa")
          (("(a)")        "a"    "\\\\1"    "\\1")
          (("(a)")        "a"    "\\\\"     "\\")
          (("(a)")        "a"    "\\&"      "a")
          (("(a)")        "a"    "\\0"      "a")
          (("([a-z]+)")   "hi z" "<\\0>"    "<hi> <z>")
          (("(a)?b")      "b"    "[\\1]"    "[]")
          (("(a)")        "a"    "\\5"      "")
          (("(a)")        "a"    "$1"       "$1")
          (("(a)")        "a"    "$$"       "$$")
          (("(a)")        "a"    "${1}"     "${1}")
          (("(a)")        "a"    "\\q"      "\\q")
          (("(a)")        "a"    "x\\"      "x\\")
          (("(a)(b)")     "ab"   "\\{2}\\{1}" "ba")
          (("(?<w>a)")    "a"    "\\{w}"    "a")
          (("(?<w>a)")    "a"    "\\{zz}"   "")
          (("(a)")        "a"    "\\{}"     "")
          (("(a)")        "a"    "\\{1"     "\\{1")
          (("a")          "aa"   "\\&\\&"   "aaaa")
          (("(a)")        "a"    "plain"    "plain"))
    "expands ~S over ~S with backslash template ~S to ~S"
    (options text template expected)
  (expect (replace-all (apply #'compile-regex options) text template
                       :template-syntax :backslash)
          :to-equal expected))

(it
  "consumes every digit of a multi-digit backslash group reference"
  (let ((regex (compile-regex "(a)(b)(c)(d)(e)(f)(g)(h)(i)(j)(k)(l)")))
    (expect (replace-all regex "abcdefghijkl" "\\12-\\11" :template-syntax :backslash)
            :to-equal "l-k")
    ;; Greedy like cl-ppcre: `\12` is group 12, never group 1 followed by "2".
    (expect (replace-all regex "abcdefghijkl" "\\1" :template-syntax :backslash)
            :to-equal "a")))

;;; The default must not move. Each row is asserted twice: once with the
;;; keyword omitted entirely, once with an explicit :DOLLAR.

(it-each ((("(?<word>a)") "a" "$$${word}-$missing-${2}-$" "$a---$")
          (("(?<word>a)") "a" "$"        "$")
          (("(?<word>a)") "a" "$wordx"   "")
          (("(?<word>a)") "a" "${}"      "")
          (("(?<word>a)") "a" "${word"   "${word")
          (("(?<word>a)") "a" "$-"       "$-")
          (("(?<a.b>x)")  "x" "$a.b"     ".b")
          (("(?<a.b>x)")  "x" "${a.b}"   "x")
          ;; The regression that motivated :BACKSLASH: under the default a
          ;; cl-ppcre-style backreference is emitted verbatim, silently.
          (("(a)")        "a" "\\1"      "\\1")
          (("(a)")        "a" "\\&"      "\\&")
          (("(a)")        "a" "\\0"      "\\0")
          (("(a)")        "a" "\\\\"     "\\\\"))
    "keeps ~S over ~S with default dollar template ~S expanding to ~S"
    (options text template expected)
  (let ((regex (apply #'compile-regex options)))
    (expect (replace-all regex text template) :to-equal expected)
    (expect (replace-all regex text template :template-syntax :dollar) :to-equal expected)))

(it
  "offers the template dialect on every replacement entry point"
  (let ((regex (compile-regex "(a)")))
    (expect (replace-first regex "aa" "[\\1]" :template-syntax :backslash)
            :to-equal "[a]a")
    (expect (replace-n regex "aa" "[\\1]" 1 :template-syntax :backslash)
            :to-equal "[a]a")
    (expect (replace-all regex "aa" "[\\1]" :template-syntax :backslash)
            :to-equal "[a][a]")
    (expect (replace-first regex "aa" "[$1]") :to-equal "[a]a")
    (expect (replace-n regex "aa" "[$1]" 1) :to-equal "[a]a")
    (expect (replace-all regex "aa" "[$1]") :to-equal "[a][a]")))

(it
  "rejects an unknown template dialect before replacing anything"
  (let ((regex (compile-regex "a")))
    (dolist (operation
        (list
          (lambda ()
            (replace-all regex "a" "b" :template-syntax :perl))
          (lambda ()
            (replace-first regex "a" "b" :template-syntax :perl))
          (lambda ()
            (replace-n regex "a" "b" 0 :template-syntax :perl))
          (lambda ()
            (replace-all regex "a" "b" :template-syntax nil))
          (lambda ()
            (replace-all regex "a" "b" :template-syntax "backslash"))))
      (signals type-error (funcall operation)))))

(it
  "ignores the template dialect when the replacement is a function"
  (let ((regex (compile-regex "(a)")))
    (dolist (syntax '(:dollar :backslash))
      (expect
        (replace-all regex "a"
                     (lambda (result text) (match-group-string result 1 text))
                     :template-syntax syntax)
        :to-equal "a"))))

;;; Octet templates keep both dialects in step with the character domain.

(it
  "expands backslash templates in the byte domain"
  (flet ((octets (&rest values)
           (make-array (length values) :element-type '(unsigned-byte 8)
                                       :initial-contents values)))
    (let ((regex (compile-byte-regex "(?<w>A)(B)")))
      (dolist (case (list
              ;; template octets, expected octets
              (list (octets #x5c #x32 #x5c #x31) '(66 65))   ; "\2\1" -> "BA"
              (list (octets #x5c #x26) '(65 66))             ; "\&"   -> "AB"
              (list (octets #x5c #x30) '(65 66))             ; "\0"   -> "AB"
              (list (octets #x5c #x5c) '(92))                ; "\\"   -> "\"
              (list (octets #x5c #x7b #x77 #x7d) '(65))      ; "\{w}" -> "A"
              (list (octets #x24 #x31) '(36 49))             ; "$1"   -> "$1"
              (list (octets #x5c #x71) '(92 113))            ; "\q"   -> "\q"
              (list (octets #x5c #x35) '())                  ; "\5"   -> ""
              (list (octets #x5c) '(92))))                   ; "\"    -> "\"
        (destructuring-bind (template expected) case
          (expect
            (coerce (replace-all regex (octets 65 66) template :template-syntax :backslash)
                    'list)
            :to-equal expected))))))

(it
  "leaves byte dollar templates exactly as they were"
  (flet ((octets (&rest values)
           (make-array (length values) :element-type '(unsigned-byte 8)
                                       :initial-contents values)))
    (let ((regex (compile-byte-regex "(?<w>A)(B)")))
      (expect (coerce (replace-all regex (octets 65 66) (octets #x24 #x31)) 'list)
              :to-equal '(65))
      (expect (coerce (replace-all regex (octets 65 66) (octets #x24 #x7b #x77 #x7d)) 'list)
              :to-equal '(65))
      ;; The byte-domain form of the motivating regression.
      (expect (coerce (replace-all regex (octets 65 66) (octets #x5c #x31)) 'list)
              :to-equal '(92 49))
      (expect (coerce (replace-all regex (octets 65 66) (octets #x5c #x26)) 'list)
              :to-equal '(92 38)))))
