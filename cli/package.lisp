;;;; cli/package.lisp
;;;;
;;;; cl-regex-kit-grep: a small `grep`-alike over cl-regex-kit, demonstrating
;;;; the library as a real command-line tool and built directly on cl-cli
;;;; (https://github.com/nerima-lisp/cl-cli) -- no adapter layer, its
;;;; declarative make-app/make-option/make-positional/run-app API is used as
;;;; documented.
(defpackage #:cl-regex-kit/cli
  (:use #:cl)
  (:import-from #:cl-regex-kit #:compile-regex #:is-match-p)
  (:import-from #:cl-cli
   #:make-app #:make-option #:make-positional #:run-app
   #:option-value #:positional-value #:invocation-stdout #:invocation-stderr)
  (:export #:main))
