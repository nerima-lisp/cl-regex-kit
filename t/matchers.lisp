;;;; t/matchers.lisp
;;;;
;;;; Project-specific cl-weave matchers, so specs read as regex assertions
;;;; instead of the standard-library boolean matchers layered onto them.
(in-package #:cl-regex-kit/test)

(defmatcher :to-match-regex (actual expected)
  "ACTUAL, a string or octet vector, is matched by EXPECTED, a compiled
REGEX or BYTE-REGEX."
  (is-match-p (expected-one expected :to-match-regex) actual))

(defmacro it-match-cases (description pattern &key matches non-matches)
  "Register one matcher specification for a pattern and its examples.

The pattern is compiled once per specification, while MATCHES and NON-MATCHES
remain declarative data at the call site."
  (let ((compiled (gensym "COMPILED-REGEX")))
    `(it ,description
       (let ((,compiled (compile-regex ,pattern)))
         ,@(mapcar (lambda (text)
                     `(expect ,text :to-match-regex ,compiled))
                   matches)
         ,@(mapcar (lambda (text)
                     `(expect-not ,text :to-match-regex ,compiled))
                   non-matches)))))

(defmacro it-parses-cases (description patterns)
  "Register one parser specification for a declarative pattern table."
  `(it ,description
     (dolist (pattern ',patterns)
       (expect (cl-regex-kit::parse-regex pattern) :to-be-truthy))))
