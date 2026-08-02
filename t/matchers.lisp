;;;; t/matchers.lisp
;;;;
;;;; Project-specific cl-weave matchers, so specs read as regex assertions
;;;; instead of the standard-library boolean matchers layered onto them.
(in-package #:cl-regex-kit/test)

(defmatcher :to-match-regex (actual expected)
  "ACTUAL, a string or octet vector, is matched by EXPECTED, a compiled
REGEX or BYTE-REGEX."
  (is-match-p (expected-one expected :to-match-regex) actual))
