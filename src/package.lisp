;;;; src/package.lisp
;;;;
;;;; The single public package. Everything a caller needs -- compiling a
;;;; pattern, matching it, and reading a match's captures -- is exported here
;;;; and nothing else.
;;;; src/package.lisp
;;;;
;;;; The single public package. Everything a caller needs -- compiling a
;;;; pattern, matching it, and reading a match's captures -- is exported here
;;;; and nothing else.
(defpackage #:cl-regex-kit
  (:use #:cl)
  (:import-from #:cl-parser-kit
   #:make-token
   #:token-type
   #:token-value
   #:token-start
   #:token-end)
  (:export
   ;; Compilation
   #:compile-regex
   #:compile-byte-regex
   #:escape
   #:regex
   #:byte-regex
   #:regex-p
   #:byte-regex-p
   #:regex-source
   #:regex-group-count
   #:regex-capture-count
   #:regex-static-capture-count
   #:regex-capture-names
   #:regex-group-index
   #:regex-advanced-p
   #:regex-advanced-step-limit
   #:regex-advanced-nest-limit
   #:regex-never-newline-p
   #:regex-callout
   ;; Multi-pattern compilation
   #:compile-regex-set
   #:compile-byte-regex-set
   #:regex-set
   #:byte-regex-set
   #:regex-set-p
   #:byte-regex-set-p
   #:regex-set-patterns
   #:regex-set-count
   #:regex-set-empty-p
   #:regex-set-matches
   #:regex-set-matches-at
   #:regex-set-matches-into
   #:regex-set-match-p
   #:regex-set-match-at-p
   ;; Matching and iteration
   #:scan
   #:scan-at
   #:captures
   #:captures-at
   #:shortest-match
   #:shortest-match-at
   #:longest-match
   #:match
   #:byte-match
   #:all-matches
   #:do-matches
   #:do-captures
   #:is-match-p
   #:is-match-at
   #:full-match
   #:full-match-p
   #:regex-capture-locations
   #:capture-locations
   #:capture-locations-p
   #:capture-locations-count
   #:capture-location-start
   #:capture-location-end
   #:scan-captures-into
   #:scan-captures-into-at
   #:run-advanced-regex
   ;; Text transformation
   #:split
   #:split-terminator
   #:split-inclusive
   #:split-n
   #:replace-first
   #:replace-all
   #:replace-n
   ;; Match results
   #:match-result
   #:match-result-p
   #:match-start
   #:match-end
   #:match-mark
   #:match-string
   #:match-captures
   #:match-group-start
   #:match-group-end
   #:match-group-string
   ;; Conditions
   #:cl-regex-kit-error
   #:regex-syntax-error
   #:regex-syntax-error-pattern
   #:regex-syntax-error-position
   #:regex-syntax-error-reason
   #:regex-timeout
   #:regex-timeout-seconds
   #:advanced-regex-limit-error
   #:advanced-regex-limit-kind
   #:advanced-regex-limit
   #:advanced-regex-limit-used))
