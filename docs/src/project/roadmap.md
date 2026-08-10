# Roadmap

## Current status

The engine is implemented end to end: parsing produces the `regex-node` AST,
Thompson construction compiles it to an instruction program, and Pike's VM
executes it with leftmost-first matching and captures. `all-matches` returns
non-overlapping results while safely advancing through zero-length matches.
`all-matches-overlapping` and its callback forms expose overlapping traversal;
the next search begins one input unit after the current match's start.

## Implemented scope

1. **`parse-regex`** -- literals, concatenation, alternation, grouping
   (capturing, named, and `(?:...)`), repetition (`*`, `+`, `?`, `{m}`,
   `{m,n}`, `{m,}`,
   greedy and lazy), character classes, escapes, inline flags, and line,
   absolute, and word-boundary anchors.
2. **`compile-to-nfa`** -- Thompson construction from the AST to the `inst`
   program, one case per `regex-node` subclass.
3. **`run-pike-vm`** -- the thread-set simulation itself: sparse-set thread
   deduplication, capture-slot save/restore per thread, and leftmost-first
   priority at `:split`.
4. **`all-matches`** -- repeated `scan` calls advancing past each match.
5. **Overlapping traversal** -- `all-matches-overlapping`,
   `do-matches-overlapping`, and `do-captures-overlapping`, including empty
   matches and character/octet range handling.
6. Focused parser, compiler, VM, and public API specifications, including
   malformed patterns, capture groups, greedy/lazy quantifiers, anchors,
   classes, and zero-length multi-match advancement.
7. SBCL `sb-cover` instrumentation produces an HTML report for every
   production source file. Run `nix develop --command env
   CL_REGEX_KIT_COVERAGE_DIRECTORY="$PWD/coverage" sbcl --script
   run-coverage.lisp` to write them to `coverage/`; the Nix check enforces a
   96% expression / 92% branch
   coverage gate across handwritten source files (generated Unicode data
   files are excluded; see `run-coverage.lisp`'s
   `+generated-source-file-names+`) -- see "Known gaps" below for why the
   gate sits below 100%.
8. Shrinkable property tests cover escaping, bounded repetition, and merged
   regex-set equivalence; bounded parser fuzzing rejects only documented
   syntax errors and exposes all other failures.
9. Unicode shorthand and line-break escapes include `\\h`/`\\H`, `\\N`, `\\R`,
   and named characters such as `\\N{LATIN CAPITAL LETTER A}`. `\\R` treats
   CRLF as one consuming unit in both capture-aware and regex-set execution.
10. Chunked input adapters -- `make-regex-stream` accepts string or
    octet-vector chunks, while `all-stream-matches` and `scan-stream` consume
    Common Lisp input streams without closing them. Matching runs at an
    explicit `regex-stream-finish`/EOF barrier, so the adapter preserves the
    complete input and the ordinary matcher retains its anchor, lookaround,
    capture, and advanced-pattern semantics. `make-incremental-regex-stream`
    additionally provides bounded-memory, low-latency matching for the
    ordinary consuming NFA subset, with explicit rejection of unsupported
    zero-width, advanced, and mixed raw/Unicode consuming programs.
11. Bounded fuzzy matching for regular NFA regexes through `fuzzy-scan`,
    `fuzzy-search`, `fuzzy-match`, and `byte-fuzzy-match`, with explicit edit
    and state limits plus a reported `match-edit-distance`.

## Explicit non-goals

See [Compatibility](../reference/compatibility.md) for the split between the
regular NFA path and the bounded advanced executor. The project does not embed
Perl-style code interpolation. Fuzzy matching is intentionally limited to the
regular NFA path; advanced ordered-backtracking fuzzy semantics remain outside
the current dialect.

## Known gaps

- **Coverage is gated at 96% expression / 92% branch coverage.** The Nix
  check parses a non-empty generated `sb-cover` report and fails below either
  threshold. The target is intentionally below 100%/100% because SB-COVER
  does not mark forms such as `in-package`, value-less
  `defvar`/`defconstant`, `defmacro`/`defclass` bodies, `defparameter` data
  literals, or `&key` defaults as executed. Defensive catch-all errors also
  remain valuable when the current enum is exhaustive; they should not be
  deleted to improve a metric.
- Coverage is release evidence, not a substitute for semantic tests. When a
  generated report identifies a reachable path, add a behavior-level
  regression or property case before changing the implementation. Measure
  the current tree with `CL_REGEX_KIT_COVERAGE_DIRECTORY=$(mktemp -d)
  nix develop --command sbcl --script run-coverage.lisp` rather than copying
  coverage counts into this document.

## Future extensions

- Broader PCRE-compatible syntax beyond the current advanced executor, with
  each addition requiring parser, AST, runtime, replacement, regex-set, and
  semantic regression coverage
- Unicode data-version maintenance: refresh generated Unicode tables and
  re-run the UAX-29 grapheme, word, and sentence boundary fixtures when
  adopting a newer Unicode version; the current boundary rules pass the
  Unicode 16.0.0 conformance files
- Broader incremental semantics beyond the current consuming NFA subset:
  zero-width and end-of-input assertions, line-break sequences, lookaround,
  advanced ordered-backtracking constructs, and overlapping incremental
  traversal each need a separately specified contract before they can be
  supported without weakening correctness.
