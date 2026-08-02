# Roadmap

## Current status

The engine is implemented end to end: parsing produces the `regex-node` AST,
Thompson construction compiles it to an instruction program, and Pike's VM
executes it with leftmost-first matching and captures. `all-matches` returns
non-overlapping results while safely advancing through zero-length matches.

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
5. Focused parser, compiler, VM, and public API specifications, including
   malformed patterns, capture groups, greedy/lazy quantifiers, anchors,
   classes, and zero-length multi-match advancement.
6. SBCL `sb-cover` instrumentation produces an HTML report for every
   production source file. Run `nix run .#coverage` to write them to
   `coverage/`; the Nix check enforces 100% expression and 100% branch
   coverage across handwritten source files (generated Unicode data files are
   excluded; see `run-coverage.lisp`'s `+generated-source-file-names+`).
7. Shrinkable property tests cover escaping, bounded repetition, and merged
   regex-set equivalence; bounded parser fuzzing rejects only documented
   syntax errors and exposes all other failures.

## Explicit non-goals

See [Compatibility](../reference/compatibility.md): backreferences and lookaround
are not planned, by design.

## Known gaps

- **Coverage sits below the 100%/100% gate** (95.63% expression / 93.38%
  branch as of this writing). Most of the remaining gap is `sb-cover` not
  crediting `in-package`, value-less `defvar`/`defconstant`,
  `defmacro`/`defclass` bodies, `defparameter` data literals, or `&key`
  defaults as "executed" regardless of how thoroughly the surrounding file
  is exercised -- confirmed by `scan`'s `(start 0)` default still showing
  uncovered even though several different test files already call it
  without `:start`. A remaining, likely-unreachable branch is
  `ensure-byte-character`'s own `> #xff` check in
  regex-grammar-support.lisp: every call site that reaches it
  (`ensure-byte-class-character`, `range`) either already rejects a
  non-raw-octet character above `#x7f` first, or only ever passes bounds
  that are structurally `<= #xff` (`\xHH` and `\ooo` escapes cannot
  produce a larger value); no pattern syntax was found that reaches it
  with a larger, raw-octet value.
- **`checks.benchmark` fails independent of any of this branch's own
  changes**: `nix flake check`'s `mkTestApp`-driven build of the benchmark
  app compiles `run-benchmarks.lisp` against a read-only Nix store
  checkout and gets `SB-INT:SIMPLE-FILE-ERROR "Permission denied"`
  writing a fasl beside the source, reproduced identically against the
  script's unmodified, pre-refactor form. An ASDF output-translations
  redirect (scoped to the checkout's own subtree, `:inherit-configuration`
  for everything else) did not resolve it and was not kept; whatever
  `mkTestApp`'s own fasl-cache wiring assumes about its build environment
  needs its own investigation, most likely in `cl-nix-forge` rather than
  in this repository.

## Future extensions

- Additional regular-expression syntax where it preserves the finite-automaton
  execution model
