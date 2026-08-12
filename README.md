# cl-regex-kit

[![CI](https://github.com/nerima-lisp/cl-regex-kit/actions/workflows/ci.yml/badge.svg?branch=main)](https://github.com/nerima-lisp/cl-regex-kit/actions/workflows/ci.yml)
[![License: MIT](https://img.shields.io/badge/License-MIT-blue.svg)](LICENSE)
[![Documentation](https://img.shields.io/badge/docs-MkDocs%20Material-0a7a5a)](https://nerima-lisp.github.io/cl-regex-kit/)

`cl-regex-kit` is a from-scratch regular expression engine for SBCL, built on
Thompson NFA construction and Pike's VM -- the architecture behind RE2 and
Rust's `regex` crate -- so, for a fixed compiled program, matching stays linear
in the input length instead of risking the catastrophic backtracking of a naive
engine. Patterns that fit the finite-automaton model retain that path's
linear-time behavior. Patterns that require capture-dependent or
ordered-backtracking semantics use a separate bounded advanced executor
instead of being silently compiled with different semantics.

Full documentation is published at <https://nerima-lisp.github.io/cl-regex-kit/>.
The source for that site lives in [docs/src/](docs/src/).

**Status:** the parser, NFA compiler, Pike's VM matcher, and advanced
ordered-backtracking executor are implemented.
Supported syntax includes literals, alternation, capturing and non-capturing
groups, named captures, reusable capture-location buffers, literal escaping,
greedy and lazy repetition (including
`{m}`, `{m,n}`, and `{m,}`), Unicode-aware
  `\\d`/`\\w`/`\\s` and Rust/RE2-style word boundaries, Unicode general-category, Script,
  Script_Extensions, Block, Age, Grapheme_Cluster_Break, Word_Break, and
  Sentence_Break properties, ASCII POSIX classes, and class
  intersection/difference/symmetric-difference,
  inline `i`/`m`/`s`/`R`/`U`/`x`/`u`
flags, `.`, and line or absolute anchors. The advanced executor also supports
backreferences, lookaround, extended grapheme clusters, possessive
quantifiers, atomic groups, subroutines and recursion, conditionals,
branch-reset groups, `\K`, `\G`, `\Z`, PCRE-style callouts, and control verbs.
Advanced
execution is bounded by `:size-limit` and `:nest-limit`; it does not provide the
NFA path's linear-time guarantee for arbitrary patterns. See the
[compatibility guide](https://nerima-lisp.github.io/cl-regex-kit/reference/compatibility/)
for the exact syntax and execution boundary.

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
`(match-result text)` at each match. Templates use Rust-style `$0`, `$1`,
`$name`, `${name}`, and `$$`; a backslash is literal text.

`fuzzy-scan` supports bounded insertions, deletions, and
substitutions for regular NFA regexes. Use `fuzzy-match` or
`byte-fuzzy-match` for compile-on-demand character or octet-vector matching;
`match-edit-distance` reports the selected edit count.

For overlapping search, use `all-matches-overlapping`,
`do-matches-overlapping`, or `do-captures-overlapping`; these operate on a
complete string or octet-vector value and report zero-width matches at every
eligible input position. `do-matches` and `do-captures` are callback iteration,
not chunked streaming matchers. For input that arrives in pieces, use the
low-latency `make-incremental-regex-stream` API for the ordinary consuming NFA
subset, or use `make-regex-stream` with `regex-stream-feed` and
`regex-stream-finish`, or use
`all-stream-matches`/`scan-stream` with a Common Lisp input stream. Chunks are
copied into an owned buffer and matching starts at `finish`/EOF so anchors,
lookaround, and advanced patterns remain correct across boundaries. The
stream state retains that buffer until `regex-stream-reset` or garbage
collection; this is a chunked input adapter, not a low-latency bounded-memory
matcher. The incremental API retains matcher state but not input; callers must
retain or assemble chunks when they need to resolve result substrings. It
rejects zero-width, advanced, anchor, lookaround, `\\R`, and byte-Unicode
patterns; raw byte protocols should use an explicit `(?-u:...)` scope.

`compile-regex-set`/`compile-byte-regex-set` and the `regex-set`/`byte-regex-set`
macros compile NFA-compatible patterns into one merged NFA for RE2/Rust-style
multi-pattern matching: `regex-set-matches`, `regex-set-match-p`, and their
`-at`/`-into` variants report which member patterns matched. Members requiring
advanced ordered backtracking remain supported, but are evaluated individually;
a set containing such members does not promise one input scan for every member.

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
  url = "github:nerima-lisp/cl-regex-kit/v2.0.0";
  inputs.nixpkgs.follows = "nixpkgs";
};
```

Note the pinned tag. Consumers inside this org must pin a release tag rather
than follow the default branch. On a `lispDependencies` edge, read
`cl-regex-kit.packages.<system>.cl-regex-kit` -- `packages.default` is the
`cl-regex-kit-grep` binary described under [Command Line](#command-line), not
the ASDF system.

Without Nix, put the repository where ASDF can find it and evaluate
`(asdf:load-system "cl-regex-kit")`. The library depends on
[`cl-parser-kit`](https://github.com/nerima-lisp/cl-parser-kit) and
[`cl-concurrent-kit`](https://github.com/nerima-lisp/cl-concurrent-kit); the
test system also needs [`cl-weave`](https://github.com/nerima-lisp/cl-weave),
[`cl-json-kit`](https://github.com/nerima-lisp/cl-json-kit),
[`cl-codec-kit`](https://github.com/nerima-lisp/cl-codec-kit), and
[`cl-dataflow`](https://github.com/nerima-lisp/cl-dataflow),
and the optional command-line system needs
[`cl-cli`](https://github.com/nerima-lisp/cl-cli).

## Command Line

The optional `cl-regex-kit/cli` ASDF system builds `cl-regex-kit-grep`, a small
grep-compatible command that uses this engine directly. It is what this flake
delivers as `packages.default`, so a bare `nix build` in a checkout produces
it:

```sh
nix build              # -> ./result/bin/cl-regex-kit-grep
./result/bin/cl-regex-kit-grep -n 'error|warning' application.log
```

Or, once it is on `PATH`:

```sh
cl-regex-kit-grep -n 'error|warning' application.log
cl-regex-kit-grep -c '\d+' first.txt second.txt
```

Run `cl-regex-kit-grep --help` for the supported options. Exit status is `0`
when at least one selected line exists, `1` when none exists, and `2` when an
input cannot be read.

## Documentation

- [Getting started](https://nerima-lisp.github.io/cl-regex-kit/getting-started/)
- [Core concepts](https://nerima-lisp.github.io/cl-regex-kit/guide/core-concepts/) --
  the parser -> NFA -> Pike's VM pipeline
- [API reference](https://nerima-lisp.github.io/cl-regex-kit/reference/api/)
- [Architecture](https://nerima-lisp.github.io/cl-regex-kit/reference/architecture/)

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
- **Public API:** `api-regex.lisp`, `api-compile.lisp`,
  `api-match-support.lisp`, `api-match.lisp`, `api-operations.lisp`, and
  `api-replace.lisp` provide compiled regexes and operations.
  `regex-set-compile.lisp` and `regex-set-match.lisp` provide the regex-set
  API. `advanced-match.lisp` contains the bounded AST evaluator, while
  `advanced-runner.lisp` owns the public timeout boundary and leftmost-result
  entry point.

Unicode property domains depend on SBCL's Unicode tables. Enumerating finite
runtime domains also inspects internal `SB-KERNEL` function return-type
metadata and requires a finite `MEMBER` type. Those metadata interfaces are not
stable public SBCL APIs; an incompatible SBCL upgrade therefore makes Unicode
property resolution fail explicitly instead of silently using an incomplete
domain. Validate SBCL upgrades with the full test suite.

## Development

The flake exposes `x86_64-linux` and `aarch64-darwin` outputs. CI gates only
the `x86_64-linux` checks; `aarch64-darwin` is the development platform and is
not part of the CI gate.

```sh
nix develop          # SBCL with CL_SOURCE_REGISTRY already set
nix build            # -> ./result/bin/cl-regex-kit-grep
nix run .#test       # run the test suite
nix run .#benchmark  # run the benchmark suite with configurable defaults
nix develop --command env CL_REGEX_KIT_COVERAGE_DIRECTORY="$PWD/coverage" \
  sbcl --script run-coverage.lisp  # write an HTML coverage report locally
nix flake check      # tests + benchmark + formatting + docs, the CI gate
nix fmt              # format Nix sources (treefmt)
```

Tests live in `t/` and run under [cl-weave](https://github.com/nerima-lisp/cl-weave),
with repository-local declarative case macros for the grep/byte matrix tests.
`nix run .#test` provides the required dependencies; direct
`sbcl --script run-tests.lisp` also requires ASDF to resolve
`cl-parser-kit`, `cl-concurrent-kit`, `cl-weave`, `cl-json-kit`,
`cl-codec-kit`, `cl-dataflow`, and `cl-cli`.
The suite combines focused examples with shrinkable property tests and bounded
parser fuzzing, so failures retain a minimal reproducible input.
`nix flake check -L` recompiles the production sources with SBCL's `sb-cover`
as its coverage check and validates the generated report. The local command
above writes `cover-index.html` and the per-file reports to `./coverage/`.
That check gates at 96% expression / 92% branch coverage across the
handwritten production sources, so a real regression in test reachability
cannot slip through; see [the roadmap](https://nerima-lisp.github.io/cl-regex-kit/project/roadmap/#known-gaps)
for why the gate sits below 100%.

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

Benchmark reports are emitted as JSON only. `CL_REGEX_KIT_BENCH_OUTPUT_FORMAT`
accepts `json` when set and rejects legacy Lisp or s-expression output modes.

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
compile or match operation runs under a per-operation timeout recorded in the
report metadata as `:per-operation-timeout-seconds`. The seed makes generated
input repeatable and the suite keeps workload order fixed. Each result
includes the raw samples plus median, minimum, and maximum elapsed time and
bytes consed. The report itself is not deterministic: timings, allocation
figures, runtime metadata, and host metadata can vary under natural garbage
collection. Treat it as a diagnostic baseline for repeated runs in the same
controlled environment, not as evidence of speed superiority or a valid
cross-machine comparison.

## Contributing

See the org-wide [CONTRIBUTING](https://github.com/nerima-lisp/.github/blob/main/CONTRIBUTING.md)
guide and the [package standard](https://github.com/nerima-lisp/.github/blob/main/PACKAGE_STANDARD.md).

## Support

See [SUPPORT](https://github.com/nerima-lisp/.github/blob/main/SUPPORT.md).

## License

MIT. See [LICENSE](LICENSE).
