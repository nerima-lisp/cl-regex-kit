;;;; cl-regex-kit.asd
;;;; This form comes FIRST, before any defsystem. ASDF binds *package* to
;;;; ASDF-USER only for a file it loads itself; read any other way -- a REPL
;;;; `load`, an editor evaluating the buffer, flake.nix parsing :version -- the
;;;; file is read in whatever package happens to be current. Saying it makes
;;;; the file self-contained.
(in-package #:asdf-user)

(asdf:defsystem "cl-regex-kit"
  :description "A from-scratch regular expression engine for Common Lisp, built on Thompson NFA construction and Pike's VM for linear-time matching"
  :long-description "cl-regex-kit compiles a pattern to an AST, then to a Thompson-constructed NFA program, and matches the regular subset with a Pike VM thread simulation -- the architecture used by RE2 and the Rust regex crate -- preserving linear-time behavior for a fixed compiled program. Constructs that require non-regular semantics, including backreferences and lookaround, use a separate bounded ordered-backtracking executor with explicit resource limits."
  :author "takeokunn <bararararatty@gmail.com>"
  :maintainer "takeokunn <bararararatty@gmail.com>"
  :license "MIT"
  :version "2.0.0"
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
               (:file "unicode-indic-conjunct-break-data")
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
               (:file "regex-grammar-groups")
               (:file "regex-grammar-atoms")
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
               (:file "api-regex")
               (:file "api-compile")
               (:file "advanced-state")
               (:file "advanced-input")
               (:file "advanced-grapheme")
               (:file "advanced-captures")
               (:file "advanced-boundaries")
               (:file "advanced-assertions")
               (:file "advanced-structure")
               (:file "advanced-match")
               (:file "advanced-search")
               (:file "advanced-runner")
               (:file "api-match-support")
               (:file "api-match")
               (:file "fuzzy-runner")
               (:file "fuzzy-match")
               (:file "api-operations")
               (:file "streaming")
               (:file "incremental-streaming")
               (:file "api-replace")
               (:file "regex-set-compile")
               (:file "regex-set-match"))
  :in-order-to ((test-op (test-op "cl-regex-kit/test"))))

(asdf:defsystem "cl-regex-kit/cli"
  :description "cl-regex-kit-grep: a small grep built directly on cl-regex-kit"
  :author "takeokunn <bararararatty@gmail.com>"
  :maintainer "takeokunn <bararararatty@gmail.com>"
  :license "MIT"
  :version "2.0.0"
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
  :version "2.0.0"
  :homepage "https://github.com/nerima-lisp/cl-regex-kit"
  :bug-tracker "https://github.com/nerima-lisp/cl-regex-kit/issues"
  :source-control (:git "https://github.com/nerima-lisp/cl-regex-kit.git")
  :depends-on ("cl-regex-kit" "cl-regex-kit/cli" "cl-regex-kit/benchmark" "cl-weave"
               "cl-json-kit" "cl-codec-kit")
  :pathname "t"
  :serial t
  :components ((:file "package")
               (:file "matchers")
               (:file "matchers-advanced")
               (:file "unicode-matchers")
               (:file "unicode-property-matchers")
               (:file "ast-test")
               (:file "parser-test")
               (:file "nfa-test")
               (:file "pike-vm-test")
               (:file "api-test")
               (:file "api-advanced-test")
               (:file "api-unicode-test")
               (:file "api-properties-test")
               (:file "api-options-test")
               (:file "api-operations-test")
               (:file "api-operations-byte-test")
               (:file "api-regex-set-test")
               (:file "api-regex-set-execution-test")
               (:file "streaming-test")
               (:file "incremental-streaming-test")
               (:file "api-replace-test")
               (:file "api-fuzzy-test")
               (:file "benchmark-test")
               (:file "advanced-edge-test")
               (:file "advanced-boundary-test")
               (:file "advanced-cases-test")
               (:file "advanced-cases-boundary-test")
               (:file "advanced-cases-control-test")
               (:file "advanced-cases-evaluator-test")
               (:file "advanced-cases-state-test")
               (:file "ast-edge-test")
               (:file "tokenizer-edge-test")
               (:file "cli-test")
               )
  :perform (test-op
            (operation component)
            (declare (ignore operation component))
            (unless (funcall
                     (symbol-function
                      (find-symbol "RUN-TESTS" "CL-REGEX-KIT/TEST")))
              (error "cl-regex-kit test suite failed"))))

(asdf:defsystem "cl-regex-kit/benchmark"
  :description "Benchmark system for cl-regex-kit"
  :depends-on ("cl-regex-kit" "cl-dataflow-kit" "cl-json-kit")
  :pathname "benchmarks"
  :serial t
  :components ((:file "package") (:file "suite")))
