# Roadmap

## Current status

The engine is implemented end to end: parsing produces the `regex-node` AST,
Thompson construction compiles it to an instruction program, and Pike's VM
executes it with leftmost-first matching and captures. `all-matches` returns
non-overlapping results while safely advancing through zero-length matches.
Patterns that require capture-dependent or ordered-backtracking semantics are
dispatched to the bounded AST executor, which is part of the normal ASDF load
path and exposes explicit step and nesting limits. `all-matches-overlapping`
and its callback forms expose overlapping traversal; the next search begins
one input unit after the current match's start.

## Implemented scope

1. **`parse-regex`** -- literals, concatenation, alternation, grouping
   (capturing, named, and `(?:...)`), repetition (`*`, `+`, `?`, `{m}`,
   `{m,n}`, `{m,}`,
   greedy and lazy), character classes and set operations (including
   Perl/PCRE2-style extended classes `(?[...])` with grouped expressions,
   escapes, and POSIX atoms), escapes, inline flags, and line, absolute, and
   word-boundary anchors.
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
   coverage gate across handwritten source files (the generated Unicode data
   tables and the declarative `defpackage` file are excluded; see
   `run-coverage.lisp`'s `+generated-source-file-names+`) -- see "Known gaps"
   below for why the gate sits below 100%.
8. Shrinkable property tests cover escaping, bounded repetition, and merged
   regex-set equivalence; bounded parser fuzzing rejects only documented
   syntax errors and exposes all other failures.
9. Unicode shorthand and line-break escapes include `\\h`/`\\H`, `\\N`, `\\R`,
   and named characters such as `\\N{LATIN CAPITAL LETTER A}`. `\\R` treats
   CRLF as one consuming unit in both capture-aware and regex-set execution.
10. The bounded advanced executor handles backreferences, lookaround, extended
    grapheme clusters, atomic and possessive constructs, subroutines and
    recursion, conditionals, branch-reset groups, control verbs, callouts, and
    the related public scan, replace, split, and `regex-set` operations.
11. Regex sets expose source-order matching, positional queries, boolean
    queries, and caller-owned bit-vector result buffers; the byte-oriented
    variants reuse the same operations over octet vectors.
12. Overlapping traversal is available through `all-matches-overlapping`, the
    callback iteration macros, and the stream helpers; zero-width matches are
    reported at every eligible input position.
13. Chunked input adapters -- `make-regex-stream` accepts string or
    octet-vector chunks, while `all-stream-matches` and `scan-stream` consume
    Common Lisp input streams without closing them. Matching runs at an
    explicit `regex-stream-finish`/EOF barrier, so the adapter preserves the
    complete input and the ordinary matcher retains its anchor, lookaround,
    capture, and advanced-pattern semantics. `make-incremental-regex-stream`
    additionally provides bounded-memory, low-latency matching for the
    ordinary consuming NFA subset, with explicit rejection of unsupported
    zero-width, advanced, and mixed raw/Unicode consuming programs.
14. Bounded fuzzy matching supports regular NFA patterns through `fuzzy-scan`,
    `fuzzy-search`, `fuzzy-match`, and `byte-fuzzy-match`, with explicit edit
    and state limits and a `fuzzy-match-unsupported` condition for advanced
    patterns; `match-edit-distance` reports the selected edit count.

## Explicit non-goals

See [Compatibility](../reference/compatibility.md) for the split between the
regular NFA path and the bounded advanced executor. The project does not embed
Perl-style code interpolation. Fuzzy matching is limited to regular NFA
patterns under explicit edit and state bounds; advanced ordered-backtracking
patterns signal `fuzzy-match-unsupported`.

## Known gaps

- **The coverage gate is deliberately set below 100%, and cannot be raised to
  it.** The coverage check is an enforcing gate rather than an outstanding
  failure: `nix flake check` runs it alongside the functional test derivation,
  and the flake check only succeeds when both are green. Reproduce the current
  expression and branch percentages with that command rather than relying on a
  figure quoted here. The thresholds are fixed at 96% expression and 92% branch
  coverage in `flake.nix`'s `checks.coverage` because a line-by-line read of
  every flagged file found the residual gap dominated by `sb-cover` blind spots
  that no additional test can close: `in-package` forms, value-less
  `defvar`/`defconstant`, `defmacro` and `defclass` bodies, `defparameter` data
  literals, and `&key` default forms are never marked executed however
  thoroughly the surrounding code is exercised. The remainder is a small number
  of defensive `otherwise` and catch-all `error` branches guarding
  already-exhaustive `case`/`ecase` dispatches; deleting those would trade a
  percentage-point gain for weaker protection against a future unhandled enum
  value, so they stay and the gate accommodates them. Coverage is measured over
  handwritten `src/` files only -- the generated Unicode tables and the
  declarative `defpackage` file are excluded through `run-coverage.lisp`'s
  `+generated-source-file-names+`.
- **Unicode data is current for the generated families.** The checked-in
  property, age, Word_Break, and case-folding tables are generated from the
  official Unicode 17.0.0 UCD. Reproduce the checked-in-data check with
  `perl tools/generate-unicode-data.pl --ucd-dir DIR --check` after obtaining
  the UCD source. General categories, Script, Block, Grapheme_Cluster_Break,
  Sentence_Break, and selected runtime-only binary properties still follow
  the Unicode data provided by SBCL; full implementation-independent UCD
  conformance remains future work.
- **Dialect parity remains bounded by design.** The advanced executor supports
  the documented backreference, assertion, recursion, control-verb, and
  callout families, but its resource limits are not a linear-time or
  fixed-memory guarantee. Perl-style code interpolation remains an explicit
  non-goal, and advanced ordered-backtracking patterns remain outside the
  bounded fuzzy-matching dialect.
- **Coverage is release evidence, not a substitute for semantic tests.** When
  a generated report identifies a reachable path, add a behavior-level
  regression or property case before changing the implementation. Measure the
  current tree with `CL_REGEX_KIT_COVERAGE_DIRECTORY=$(mktemp -d) nix develop
  --command sbcl --script run-coverage.lisp` rather than copying coverage
  counts into this document.

## Future extensions

- Broaden the PCRE-compatible dialect beyond the current advanced executor,
  with each addition requiring parser, AST, runtime, replacement, regex-set,
  and semantic regression coverage
- Unicode conformance expansion for grapheme, word, and sentence boundary
  behavior, including more UAX-29 edge-case fixtures
- Keep the normal ASDF and Nix test entry points green while extending backend
  coverage and documenting the supported bounds
- Broader incremental semantics beyond the current consuming NFA subset:
  zero-width and end-of-input assertions, line-break sequences, lookaround,
  advanced ordered-backtracking constructs, and overlapping incremental
  traversal each need a separately specified contract before they can be
  supported without weakening correctness
