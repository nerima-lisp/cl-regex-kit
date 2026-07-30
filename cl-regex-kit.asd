;;;; cl-regex-kit.asd
(asdf:defsystem "cl-regex-kit"
  :description "A from-scratch regular expression engine for Common Lisp, built on Thompson NFA construction and Pike's VM for linear-time matching"
  :long-description "cl-regex-kit compiles a pattern to an AST, then to a Thompson-constructed NFA program, and matches it with a Pike VM thread simulation -- the architecture used by RE2 and the Rust regex crate -- so matching time is linear in the input regardless of the pattern. Backreferences and lookaround are out of scope because they cannot be expressed by a finite automaton without giving up that guarantee."
  :author "takeokunn <bararararatty@gmail.com>"
  :maintainer "takeokunn <bararararatty@gmail.com>"
  :license "MIT"
  :version "0.1.0"
  :homepage "https://github.com/nerima-lisp/cl-regex-kit"
  :bug-tracker "https://github.com/nerima-lisp/cl-regex-kit/issues"
  :source-control (:git "https://github.com/nerima-lisp/cl-regex-kit.git")
  :depends-on ()
  :pathname "src"
  :serial t
  :components
  ((:file "package")
   (:file "conditions")
   (:file "ast")
   (:file "unicode-property-data")
   (:file "unicode-extra-binary-property-data")
   (:file "unicode-age-data")
   (:file "unicode-properties")
   (:file "unicode-case-folding-data")
   (:file "character-class")
   (:file "parser-syntax")
   (:file "parser")
   (:file "nfa")
   (:file "pike-vm")
   (:file "api")
   (:file "api-match")
   (:file "api-operations")
   (:file "api-replace")
   (:file "regex-set"))
  :in-order-to ((test-op (test-op "cl-regex-kit/test"))))

(asdf:defsystem "cl-regex-kit/test"
  :description "Test system for cl-regex-kit"
  :author "takeokunn <bararararatty@gmail.com>"
  :maintainer "takeokunn <bararararatty@gmail.com>"
  :license "MIT"
  :version "0.1.0"
  :homepage "https://github.com/nerima-lisp/cl-regex-kit"
  :bug-tracker "https://github.com/nerima-lisp/cl-regex-kit/issues"
  :source-control (:git "https://github.com/nerima-lisp/cl-regex-kit.git")
  :depends-on ("cl-regex-kit" "cl-weave")
  :pathname "t"
  :serial t
  :components
  ((:file "package")
   (:file "ast-test")
   (:file "parser-test")
   (:file "nfa-test")
   (:file "pike-vm-test")
   (:file "api-test")
   (:file "api-unicode-test")
   (:file "api-properties-test")
   (:file "api-options-test")
   (:file "api-operations-test"))
  :perform (test-op (operation component)
             (declare (ignore operation component))
             (unless (funcall (symbol-function (find-symbol "RUN-TESTS" "CL-REGEX-KIT/TEST")))
               (error "cl-regex-kit test suite failed"))))
