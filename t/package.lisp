;;;; t/package.lisp
(defpackage #:cl-regex-kit/test
  (:use #:cl)
  ;; DESCRIBE clashes with CL:DESCRIBE; nothing else needs shadowing.
  (:shadowing-import-from #:cl-weave #:describe)
  (:import-from #:cl-weave
   ;; Registration and assertions
   #:it #:it-each #:it-property #:it-fuzz
   #:expect #:signals #:run-all
   ;; Custom matcher definition
   #:defmatcher
   ;; Property generators
   #:gen-integer #:gen-string #:gen-list #:gen-member)
  (:import-from #:cl-regex-kit
   #:compile-regex #:scan #:match #:all-matches
   #:match-start #:match-end #:match-string
   #:match-group-start #:match-group-end #:match-group-string
   #:regex-syntax-error)
  (:export #:run-tests))

(in-package #:cl-regex-kit/test)

(defun run-tests ()
  "Run every registered spec, signalling on any failure so ASDF's TEST-OP fails."
  (unless (run-all :reporter :spec :timeout-ms 10000)
    (error "cl-regex-kit test suite failed"))
  (format t "~&cl-regex-kit/test: successful completion with 0 failures~%")
  t)
