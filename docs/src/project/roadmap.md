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

## Future extensions

- Additional regular-expression syntax where it preserves the finite-automaton
  execution model
