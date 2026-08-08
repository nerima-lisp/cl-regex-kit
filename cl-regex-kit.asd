;;;; cl-regex-kit.asd
;;;; This form comes FIRST, before any defsystem. ASDF binds *package* to
;;;; ASDF-USER only for a file it loads itself; read any other way -- a REPL
;;;; `load`, an editor evaluating the buffer, flake.nix parsing :version -- the
;;;; file is read in whatever package happens to be current. Saying it makes
;;;; the file self-contained.
(in-package #:asdf-user)

(asdf:defsystem "cl-regex-kit"
  :description "A from-scratch Common Lisp regular expression engine with a Thompson NFA/Pike VM for regular patterns and a bounded AST executor for advanced constructs"
  :long-description "cl-regex-kit parses patterns into an AST. Regular patterns are compiled to a Thompson NFA program and matched by a Pike VM thread simulation -- the architecture used by RE2 and the Rust regex crate -- with linear-time matching for a fixed program. Patterns with capture-dependent or ordered-backtracking constructs, such as backreferences and lookaround, use a separate AST executor with configurable step and nesting limits; the linear-time guarantee applies to the regular path, not to arbitrary advanced patterns."
  :author "takeokunn <bararararatty@gmail.com>"
  :maintainer "takeokunn <bararararatty@gmail.com>"
  :license "MIT"
  :version "0.4.0"
  :homepage "https://github.com/nerima-lisp/cl-regex-kit"
  :bug-tracker "https://github.com/nerima-lisp/cl-regex-kit/issues"
  :source-control (:git "https://github.com/nerima-lisp/cl-regex-kit.git")
  :depends-on ("cl-concurrent-kit" "cl-parser-kit")
  :pathname "src"
  :serial t
  :components ((:file "package")
               (:file "conditions")
               (:file "ast")
               (:file "unicode-property-data")
               (:file "unicode-extra-binary-property-data")
               (:file "unicode-age-data-1")
               (:file "unicode-age-data-2")
               (:file "unicode-age-data-3")
               (:file "unicode-age-data")
               (:file "unicode-binary-property-range-data")
               (:file "unicode-properties")
               (:file "unicode-property-runtime")
               (:file "unicode-property-resolver")
               (:file "unicode-case-folding-data")
               (:file "character-class")
               (:file "text-boundaries")
               (:file "parser-syntax")
               (:file "regex-tokenizer-escapes")
               (:file "regex-tokenizer")
               (:file "regex-grammar-support")
               (:file "regex-grammar")
               (:file "regex-grammar-classes")
               (:file "nfa")
               (:module
                "pike-vm-core"
                :pathname
                ""
                :serial
                t
                :components
                ((:file "pike-vm-instructions")
                 (:file "pike-vm-closure")
                 (:file "pike-vm-capture")
                 (:file "pike-vm-set")))
               (:file "api")
               (:file "api-match")
               (:file "advanced-match")
               (:file "api-operations")
               (:file "api-replace")
               (:file "regex-set"))
  :in-order-to ((test-op (test-op "cl-regex-kit/test"))))

(asdf:defsystem "cl-regex-kit/cli"
  :description "cl-regex-kit-grep: a small grep built directly on cl-regex-kit"
  :author "takeokunn <bararararatty@gmail.com>"
  :maintainer "takeokunn <bararararatty@gmail.com>"
  :license "MIT"
  :version "0.4.0"
  :homepage "https://github.com/nerima-lisp/cl-regex-kit"
  :bug-tracker "https://github.com/nerima-lisp/cl-regex-kit/issues"
  :source-control (:git "https://github.com/nerima-lisp/cl-regex-kit.git")
  :depends-on ("cl-regex-kit" "cl-cli")
  :pathname "cli"
  :serial t
  :components ((:file "package") (:file "main"))
  :build-operation "program-op"
  :build-pathname "cl-regex-kit-grep"
  :entry-point "cl-regex-kit/cli:main")

(asdf:defsystem "cl-regex-kit/test"
  :description "Test system for cl-regex-kit"
  :author "takeokunn <bararararatty@gmail.com>"
  :maintainer "takeokunn <bararararatty@gmail.com>"
  :license "MIT"
  :version "0.4.0"
  :homepage "https://github.com/nerima-lisp/cl-regex-kit"
  :bug-tracker "https://github.com/nerima-lisp/cl-regex-kit/issues"
  :source-control (:git "https://github.com/nerima-lisp/cl-regex-kit.git")
  :depends-on ("cl-regex-kit" "cl-regex-kit/cli" "cl-weave")
  :pathname "t"
  :serial t
  :components ((:file "package")
               (:file "matchers")
               (:file "ast-test")
               (:file "parser-test")
               (:file "nfa-test")
               (:file "pike-vm-test")
               (:file "api-test")
               (:file "api-unicode-test")
               (:file "api-properties-test")
               (:file "api-options-test")
               (:file "api-operations-test")
               (:file "api-operations-byte-test")
               (:file "api-replace-test")
               (:file "cli-test"))
  :perform (test-op
            (operation component)
            (declare (ignore operation component))
            (unless (funcall
                     (symbol-function
                      (find-symbol "RUN-TESTS" "CL-REGEX-KIT/TEST")))
              (error "cl-regex-kit test suite failed"))))

(asdf:defsystem "cl-regex-kit/benchmark"
  :description "Benchmark system for cl-regex-kit"
  :depends-on ("cl-regex-kit")
  :pathname "benchmarks"
  :serial t
  :components ((:file "package") (:file "suite")))
