# cl-regex-kit

[![CI](https://github.com/nerima-lisp/cl-regex-kit/actions/workflows/ci.yml/badge.svg?branch=main)](https://github.com/nerima-lisp/cl-regex-kit/actions/workflows/ci.yml)
[![License: MIT](https://img.shields.io/badge/License-MIT-blue.svg)](LICENSE)
[![Documentation](https://img.shields.io/badge/docs-MkDocs%20Material-0a7a5a)](https://nerima-lisp.github.io/cl-regex-kit/)

`cl-regex-kit` is a from-scratch regular expression engine for SBCL, built on
Thompson NFA construction and Pike's VM -- the architecture behind RE2 and
Rust's `regex` crate -- so, for a fixed compiled program, matching stays linear
in the input length instead of risking the catastrophic backtracking of a naive
engine. Backreferences and
lookaround are an explicit non-goal: they cannot be expressed by a
finite automaton without giving up that guarantee.

Full documentation is published at <https://nerima-lisp.github.io/cl-regex-kit/>.
The source for that site lives in [docs/src/](docs/src/).

**Status:** the parser, NFA compiler, and Pike's VM matcher are implemented.
Supported syntax includes literals, alternation, capturing and non-capturing
groups, named captures, reusable capture-location buffers, literal escaping,
greedy and lazy repetition (including
`{m}`, `{m,n}`, and `{m,}`), Unicode-aware
  `\\d`/`\\w`/`\\s` and Rust/RE2-style word boundaries, Unicode general-category, Script,
  Script_Extensions, Block, Age, Grapheme_Cluster_Break, Word_Break, and
  Sentence_Break properties, ASCII POSIX classes, and class
  intersection/difference/symmetric-difference,
  inline `i`/`m`/`s`/`R`/`U`/`x`/`u`
flags, `.`, and line or absolute anchors. Backreferences and lookaround remain
intentionally unsupported. See the
[Roadmap](https://nerima-lisp.github.io/cl-regex-kit/roadmap/) for the full
scope and deliberate non-goals.

Byte-oriented input is available through `compile-byte-regex` and the
`byte-regex` macro. These patterns match `(array (unsigned-byte 8) (*))`
directly. Unicode-aware constructs match valid UTF-8 scalars by default, while
`(?-u:...)` and `\\C` match raw octets, including invalid UTF-8. Byte patterns
use ASCII shorthands and case folding only inside non-Unicode scopes.
Direct non-ASCII literals and Unicode escapes (`\\x{...}`, `\\u...`, `\\U...`)
in `(?-u:...)` are encoded as UTF-8; use `\\xHH` or an octal escape for an exact
raw byte. In those scopes, character classes reject non-ASCII literals and
Unicode escapes that resolve to non-ASCII scalars.

`split`, `split-terminator`, `split-inclusive`, and `split-n` split text on
non-overlapping matches, mirroring Rust `Regex::split`/`splitn`. `replace-first`,
`replace-n`, and `replace-all` substitute a template string or a function of
`(match-result text)` at each match; `:template-syntax` selects between the
default `:dollar` (Rust-style `$1`/`${name}`/`$$`) and `:backslash`
(`cl-ppcre`-compatible `\1`/`\{name}`/`\&`/`\\`, for callers migrating from
`cl-ppcre:regex-replace-all` or exposing templates to their own users).

`compile-regex-set`/`compile-byte-regex-set` and the `regex-set`/`byte-regex-set`
macros compile several patterns into one merged NFA for RE2/Rust-style
multi-pattern matching: `regex-set-matches`, `regex-set-match-p`, and their
`-at`/`-into` variants report which member patterns matched without
compiling or scanning each one separately.

## Quick Start

```lisp
(asdf:load-system "cl-regex-kit")

(cl-regex-kit:match "a.c" "abc")
;; => a MATCH-RESULT spanning "abc"

(defparameter *number* (cl-regex-kit:regex "\\d+"))
(cl-regex-kit:is-match-p *number* "item-42")
;; => T
(cl-regex-kit:full-match-p *number* "42")
;; => T
```

## Install

```nix
# flake.nix
inputs.cl-regex-kit = {
  url = "github:nerima-lisp/cl-regex-kit/v0.2.0";
  inputs.nixpkgs.follows = "nixpkgs";
};
```

Note the pinned tag. Consumers inside this org must pin a release tag rather
than follow the default branch.

Without Nix, put the repository where ASDF can find it and evaluate
`(asdf:load-system "cl-regex-kit")`. The library depends on
[`cl-parser-kit`](https://github.com/nerima-lisp/cl-parser-kit); the test system
also needs [`cl-weave`](https://github.com/nerima-lisp/cl-weave), and the
optional command-line system needs
[`cl-cli`](https://github.com/nerima-lisp/cl-cli).

## Command Line

The optional `cl-regex-kit/cli` ASDF system builds `cl-regex-kit-grep`, a small
grep-compatible command that uses this engine directly:

```sh
cl-regex-kit-grep -n 'error|warning' application.log
cl-regex-kit-grep -c '\d+' first.txt second.txt
```

Run `cl-regex-kit-grep --help` for the supported options. Exit status is `0`
when at least one selected line exists, `1` when none exists, and `2` when an
input cannot be read.

## Documentation

- [Getting started](https://nerima-lisp.github.io/cl-regex-kit/getting-started/)
- [Core concepts](https://nerima-lisp.github.io/cl-regex-kit/concepts/) --
  the parser -> NFA -> Pike's VM pipeline
- [API reference](https://nerima-lisp.github.io/cl-regex-kit/api-reference/)
- [Architecture](https://nerima-lisp.github.io/cl-regex-kit/architecture/)

## Architecture and source map

The ASDF system loads the implementation in these broad layers:

- **Foundation:** `package.lisp`, `conditions.lisp`, and `ast.lisp` define
  packages, conditions, and the regular-expression AST.
- **Unicode data and lookup:** `unicode-*-data.lisp` files contain generated or
  static tables. `unicode-properties.lisp`, `unicode-property-runtime.lisp`,
  and `unicode-property-resolver.lisp` contain the handwritten lookup and
  resolution logic.
- **Tokenizer/grammar boundary:** `parser-syntax.lisp`,
  `regex-tokenizer-escapes.lisp`, and `regex-tokenizer.lisp` turn pattern text
  into tokens. `regex-grammar-support.lisp`, `regex-grammar.lisp`, and
  `regex-grammar-classes.lisp` consume those tokens and build the AST.
- **Compilation:** `nfa.lisp` compiles the AST into a Thompson-style instruction
  program.
- **Pike VM:** `pike-vm-instructions.lisp` implements instruction matching;
  `pike-vm-closure.lisp` computes epsilon closures; `pike-vm-capture.lisp` runs
  the capture-aware VM; and `pike-vm-set.lisp` runs merged regex-set programs.
- **Public API:** `api.lisp`, `api-match.lisp`, `api-operations.lisp`, and
  `api-replace.lisp` provide compiled regexes and operations. `regex-set.lisp`
  provides the regex-set API.

Unicode property domains depend on SBCL's Unicode tables. Enumerating finite
runtime domains also inspects internal `SB-KERNEL` function return-type
metadata and requires a finite `MEMBER` type. Those metadata interfaces are not
stable public SBCL APIs; an incompatible SBCL upgrade therefore makes Unicode
property resolution fail explicitly instead of silently using an incomplete
domain. Validate SBCL upgrades with the full test suite.

## Development

The flake currently exposes Linux (`x86_64-linux`) outputs. Run these commands
on Linux or with a configured Linux builder; a successful `nix flake check` on
another host does not execute those checks.

```sh
nix develop          # SBCL with CL_SOURCE_REGISTRY already set
nix run .#test       # run the test suite
nix run .#benchmark  # run the benchmark suite with configurable defaults
nix run .#coverage   # write an HTML coverage report to ./coverage/
nix flake check      # tests + benchmark + formatting + docs, the CI gate
nix fmt              # format Nix sources (treefmt)
```

Tests live in `t/` and run under [cl-weave](https://github.com/nerima-lisp/cl-weave),
the org's test framework; `sbcl --script run-tests.lisp` runs them without Nix.
The suite combines focused examples with shrinkable property tests and bounded
parser fuzzing, so failures retain a minimal reproducible input.
`nix run .#coverage` recompiles the production sources with SBCL's `sb-cover`,
then writes `cover-index.html`. The Nix coverage check generates the same
artifacts and requires 100% expression and 100% branch coverage across
the handwritten production sources, so instrumentation and test reachability
cannot silently regress.

## Benchmarks

Run the benchmark suite through Nix or directly with SBCL:

```sh
nix run .#benchmark
sbcl --script run-benchmarks.lisp
```

The runner accepts positive integer overrides through these environment
variables:

| Variable | Default | Purpose |
| --- | ---: | --- |
| `CL_REGEX_KIT_BENCH_ITERATIONS` | `10000` | Match iterations per measured workload |
| `CL_REGEX_KIT_BENCH_COMPILE_ITERATIONS` | `100` | Compile iterations per measured workload |
| `CL_REGEX_KIT_BENCH_WARMUP` | `1000` | Warm-up iterations before measurement |
| `CL_REGEX_KIT_BENCH_SAMPLES` | `5` | Independent measured samples per phase and workload |
| `CL_REGEX_KIT_BENCH_SEED` | `1729` | Seed for generated benchmark input |
| `CL_REGEX_KIT_BENCH_REVISION` | `unspecified` | Revision label recorded in report metadata |

For example:

```sh
CL_REGEX_KIT_BENCH_ITERATIONS=5000 \
CL_REGEX_KIT_BENCH_COMPILE_ITERATIONS=50 \
CL_REGEX_KIT_BENCH_WARMUP=500 \
CL_REGEX_KIT_BENCH_SAMPLES=5 \
CL_REGEX_KIT_BENCH_SEED=1729 \
CL_REGEX_KIT_BENCH_REVISION="$(git rev-parse HEAD)" \
nix run .#benchmark
```

`nix flake check` also runs a bounded benchmark smoke check with 100 match
iterations, 10 compile iterations, 10 warm-up iterations, one sample, seed
1729, revision label `nix-check`, and a 120-second external timeout. Each
compile or match operation also has a one-second timeout. The seed makes
generated input repeatable and the suite keeps workload order fixed. Each
result includes the raw samples plus median, minimum, and maximum elapsed time
and allocation count. The report itself is not deterministic: timings,
allocation counts, runtime metadata, and host metadata can vary under natural
garbage collection. Treat it as a diagnostic baseline for repeated runs in the
same controlled environment, not as evidence of speed superiority or a valid
cross-machine comparison.

## Contributing

See the org-wide [CONTRIBUTING](https://github.com/nerima-lisp/.github/blob/main/CONTRIBUTING.md)
guide and the [package standard](https://github.com/nerima-lisp/.github/blob/main/PACKAGE_STANDARD.md).

## Support

See [SUPPORT](https://github.com/nerima-lisp/.github/blob/main/SUPPORT.md).

## License

MIT. See [LICENSE](LICENSE).
