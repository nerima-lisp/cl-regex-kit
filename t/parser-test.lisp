;;;; t/parser-test.lisp
(in-package #:cl-regex-kit/test)

;; PARSE-REGEX (src/parser.lisp) has no implementation yet. Replace this
;; placeholder with real specs -- literals, concatenation, alternation,
;; repetition, groups, character classes, anchors -- and a REGEX-SYNTAX-ERROR
;; property test (see TEST_STANDARD.md: parsers require an "always succeeds or
;; always signals the documented condition" property) once a parse tree comes
;; back instead of an unconditional error.
(it "signals that it is not yet implemented"
  (signals error (cl-regex-kit::parse-regex "a")))
