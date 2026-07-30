;;;; src/package.lisp
;;;;
;;;; The single public package. Everything a caller needs -- compiling a
;;;; pattern, matching it, and reading a match's captures -- is exported here
;;;; and nothing else.
(defpackage #:cl-regex-kit
  (:use #:cl)
  (:export
   ;; Compilation
   #:compile-regex
   #:regex
   #:regex-p
   #:regex-source
   #:regex-group-count
   #:regex-group-index
   ;; Multi-pattern compilation
   #:compile-regex-set
   #:regex-set
   #:regex-set-p
   #:regex-set-patterns
   #:regex-set-matches
   #:regex-set-match-p
   ;; Matching and iteration
   #:scan
   #:match
   #:all-matches
   #:do-matches
   #:is-match-p
   #:full-match-p
   ;; Text transformation
   #:split
   #:replace-first
   #:replace-all
   ;; Match results
   #:match-result
   #:match-result-p
   #:match-start
   #:match-end
   #:match-string
   #:match-group-start
   #:match-group-end
   #:match-group-string
   ;; Conditions
   #:cl-regex-kit-error
   #:regex-syntax-error
   #:regex-syntax-error-pattern
   #:regex-syntax-error-position
   #:regex-syntax-error-reason))
