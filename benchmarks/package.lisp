;;;; benchmarks/package.lisp
;;;;
;;;; Benchmarks live in their own package so the benchmark system stays flat
;;;; like the library and CLI systems, matching the nerima-lisp package
;;;; standard described in the project documentation.
(defpackage #:cl-regex-kit/benchmarks
  (:use #:cl)
  (:import-from
   #:cl-dataflow
   #:make-node
   #:make-pipeline
   #:run-pipeline)
  (:import-from
   #:cl-regex-kit
   #:compile-regex
   #:compile-regex-set
   #:is-match-p
   #:regex-set-count)
  (:export #:run-benchmarks))
