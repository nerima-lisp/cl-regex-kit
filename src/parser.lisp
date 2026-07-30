;;;; src/parser.lisp
;;;;
;;;; PATTERN (a string) -> a REGEX-NODE tree (ast.lisp). Recursive descent:
;;;; alternation over concatenations, concatenation over repeated atoms, atom
;;;; is a literal, a group, a character class, `.`, or an anchor.
;;;;
;;;; NOT YET IMPLEMENTED. Left as a stub so the system loads and the test
;;;; harness runs; replace the body below with a real parser.
(in-package #:cl-regex-kit)

(defun parse-regex (pattern)
  "Parse PATTERN into a REGEX-NODE tree.
Signals REGEX-SYNTAX-ERROR on malformed input."
  (declare (ignore pattern))
  (error "PARSE-REGEX is not yet implemented."))
