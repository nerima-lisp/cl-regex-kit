;;;; t/api-operations-test.lisp
;;;;
;;;; Builder options and high-level match processing.

(in-package #:cl-regex-kit/test)

(it
  "bounds NFA program size before compilation exhausts resources"
  (signals regex-syntax-error (compile-regex "a{1000}{1000}")))

(it
  "configures compilation through builder-style keyword options"
  (expect (match-string (scan (compile-regex "cat" :case-insensitive t) "--CAT--") "--CAT--")
          :to-equal "CAT")
  (expect (match-string (scan (compile-regex "^cat$" :multi-line t)
                               (format nil "dog~%cat~%bird"))
                        (format nil "dog~%cat~%bird"))
          :to-equal "cat")
  (expect (match-string (scan (compile-regex "a.b" :dot-matches-new-line t)
                              (format nil "a~%b"))
                        (format nil "a~%b"))
          :to-equal (format nil "a~%b"))
  (expect (match-string (scan (compile-regex "a+" :swap-greed t) "aaa") "aaa") :to-equal "a")
  (expect (match-string (scan (compile-regex "a b" :ignore-whitespace t) "ab") "ab")
          :to-equal "ab")
  (expect (match-string (scan (compile-regex "\\w+" :unicode nil) "éclair") "éclair")
          :to-equal "clair")
  (expect (scan (compile-regex "." :crlf t) (string #\Return)) :to-be nil)
  (expect (scan (compile-regex "." :line-terminator #\;) ";") :to-be nil)
  (expect (scan (compile-regex "." :crlf t :line-terminator #\;) ";")
          :to-be-truthy)
  (expect (scan (compile-regex "." :crlf t :line-terminator #\;)
                (string #\Return))
          :to-be nil)
  (expect (match-string (scan (compile-regex "^right$" :multi-line t :line-terminator #\;)
                              "left;right;tail")
                        "left;right;tail")
          :to-equal "right")
  (let ((crlf-text (format nil "left~C~Cright~C~Ctail"
                           #\Return #\Newline #\Return #\Newline)))
    (expect (match-string
             (scan (compile-regex "^right$" :multi-line t :crlf t :line-terminator #\;)
                   crlf-text)
             crlf-text)
            :to-equal "right"))
  (expect (scan (compile-regex "^right$" :multi-line t :crlf t :line-terminator #\;)
                "left;right;tail")
          :to-be nil)
  (expect (scan (compile-regex "a" :size-limit 4) "a") :to-be-truthy)
  (signals regex-syntax-error (compile-regex "a" :size-limit 3))
  (signals regex-syntax-error (compile-regex "a{20}" :size-limit 5))
  (signals type-error (compile-regex "a" :size-limit 0))
  (signals regex-syntax-error (compile-regex "((a))" :nest-limit 1))
  (signals type-error (compile-regex "a" :nest-limit -1))
  (signals type-error (compile-regex "a" :line-terminator "newline")))

(it
  "iterates, splits, and replaces without rescanning text manually"
  (let* ((regex (compile-regex "(?<key>[a-z]+)=(?<value>\\d+)"))
         (text "x=1,y=22")
         (starts nil))
    (do-matches (result regex text) (push (match-start result) starts))
    (expect (nreverse starts) :to-equal '(0 4))
    (expect (split (compile-regex ",") text) :to-equal '("x=1" "y=22"))
    (expect (split (compile-regex ",") text :limit 1) :to-equal '("x=1,y=22"))
    (expect
      (split (compile-regex ",") "pre,a,b" :start 4)
      :to-equal
      '("pre,a" "b"))
    (expect
      (split (compile-regex ",") "pre,a,b" :start 4 :limit 1)
      :to-equal
      '("pre,a,b"))
    (dolist (operation
             (list (lambda () (all-matches (compile-regex "a") "a" :start 2))
                   (lambda () (split (compile-regex "a") "a" :start 2))
                   (lambda () (replace-all (compile-regex "a") "a" "b" :start 2))))
      (signals error (funcall operation)))
    (expect (split (compile-regex "") "ab") :to-equal '("" "a" "b" ""))
    (expect (replace-first regex text "$value:$key") :to-equal "1:x,y=22")
    (expect
      (replace-all regex text "\${value}-$0-$$")
      :to-equal
      "1-x=1-$,22-y=22-$")
    (expect
      (replace-all
        regex
        text
        (lambda (result source)
          (string-upcase (match-group-string result "key" source))))
      :to-equal
      "X,Y")))
