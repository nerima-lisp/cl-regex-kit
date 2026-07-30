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
   #:compile-regex #:compile-byte-regex #:escape #:scan #:scan-at #:captures #:captures-at #:shortest-match #:shortest-match-at #:longest-match #:match #:byte-match #:is-match-p #:is-match-at #:all-matches #:do-matches #:do-captures
   #:full-match
   #:split #:split-terminator #:split-inclusive #:split-n #:replace-first #:replace-all #:replace-n
   #:match-start #:match-end #:match-string
   #:match-captures
   #:match-group-start #:match-group-end #:match-group-string
   #:regex-capture-locations #:capture-locations-count
   #:capture-location-start #:capture-location-end #:scan-captures-into #:scan-captures-into-at
   #:regex-p #:byte-regex-p #:regex-group-count #:regex-capture-count #:regex-static-capture-count
   #:regex-capture-names #:regex-group-index
   #:compile-regex-set #:compile-byte-regex-set #:regex-set #:byte-regex-set
   #:regex-set-p #:byte-regex-set-p #:regex-set-patterns
   #:regex-set-count #:regex-set-empty-p
   #:regex-set-matches #:regex-set-matches-at #:regex-set-matches-into
   #:regex-set-match-p #:regex-set-match-at-p
   #:regex-syntax-error #:regex-timeout)
  (:export #:run-tests))

(in-package #:cl-regex-kit/test)

(defun run-tests ()
  "Run every registered spec, signalling on any failure so ASDF's TEST-OP fails."
  ;; Unicode property coverage instruments a large generated table under SB-COVER.
  (unless (run-all :reporter :spec :timeout-ms 30000)
    (error "cl-regex-kit test suite failed"))
  (format t "~&cl-regex-kit/test: successful completion with 0 failures~%")
  t)
