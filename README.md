# cl-regex-kit

[![CI](https://github.com/nerima-lisp/cl-regex-kit/actions/workflows/ci.yml/badge.svg?branch=main)](https://github.com/nerima-lisp/cl-regex-kit/actions/workflows/ci.yml)
[![License: MIT](https://img.shields.io/badge/License-MIT-blue.svg)](LICENSE)
[![Documentation](https://img.shields.io/badge/docs-MkDocs%20Material-0a7a5a)](https://nerima-lisp.github.io/cl-regex-kit/)

`cl-regex-kit` is a from-scratch regular expression engine for SBCL, built on
Thompson NFA construction and Pike's VM -- the architecture behind RE2 and
Rust's `regex` crate -- so matching stays linear in the input length instead of
risking the catastrophic backtracking of a naive engine. Backreferences and
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
  url = "github:nerima-lisp/cl-regex-kit/v0.1.0";
  inputs.nixpkgs.follows = "nixpkgs";
};
```

Note the pinned tag. Consumers inside this org must pin a release tag rather
than follow the default branch.

Without Nix, put the repository where ASDF can find it and evaluate
`(asdf:load-system "cl-regex-kit")`. There are no runtime dependencies; only
the test system needs `cl-weave`.

## Documentation

- [Getting started](https://nerima-lisp.github.io/cl-regex-kit/getting-started/)
- [Core concepts](https://nerima-lisp.github.io/cl-regex-kit/concepts/) --
  the parser -> NFA -> Pike's VM pipeline
- [API reference](https://nerima-lisp.github.io/cl-regex-kit/api-reference/)
- [Architecture](https://nerima-lisp.github.io/cl-regex-kit/architecture/)

## Development

```sh
nix develop          # SBCL with CL_SOURCE_REGISTRY already set
nix run .#test       # run the test suite
nix run .#coverage   # write an HTML coverage report to ./coverage/
nix flake check      # tests + formatting + docs, the same gate CI uses
nix fmt              # format Nix sources (treefmt)
```

Tests live in `t/` and run under [cl-weave](https://github.com/nerima-lisp/cl-weave),
the org's test framework; `sbcl --script run-tests.lisp` runs them without Nix.
The suite combines focused examples with shrinkable property tests and bounded
parser fuzzing, so failures retain a minimal reproducible input.
`nix run .#coverage` recompiles the production sources with SBCL's `sb-cover`,
then writes `cover-index.html`. The Nix coverage check generates the same
artifacts and requires at least 90% expression and 85% branch coverage across
the handwritten production sources, so instrumentation and test reachability
cannot silently regress.

## Contributing

See the org-wide [CONTRIBUTING](https://github.com/nerima-lisp/.github/blob/main/CONTRIBUTING.md)
guide and the [package standard](https://github.com/nerima-lisp/.github/blob/main/PACKAGE_STANDARD.md).

## Support

See [SUPPORT](https://github.com/nerima-lisp/.github/blob/main/SUPPORT.md).

## License

MIT. See [LICENSE](LICENSE).
