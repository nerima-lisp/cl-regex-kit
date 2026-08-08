(defpackage #:cl-regex-kit/benchmarks (:use #:cl)
  (:import-from
   #:cl-regex-kit
   #:compile-regex
   #:compile-regex-set
   #:is-match-p
   #:regex-set-count)
  (:export #:run-benchmarks))
