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
   production source file. Run `nix develop --command env
   CL_REGEX_KIT_COVERAGE_DIRECTORY="$PWD/coverage" sbcl --script
   run-coverage.lisp` to write them to `coverage/`; the Nix check enforces a
   96% expression / 92% branch
   coverage gate across handwritten source files (generated Unicode data
   files are excluded; see `run-coverage.lisp`'s
   `+generated-source-file-names+`) -- see "Known gaps" below for why the
   gate sits below 100%.
7. Shrinkable property tests cover escaping, bounded repetition, and merged
   regex-set equivalence; bounded parser fuzzing rejects only documented
   syntax errors and exposes all other failures.
8. Unicode shorthand and line-break escapes include `\\h`/`\\H`, `\\N`, `\\R`,
   and named characters such as `\\N{LATIN CAPITAL LETTER A}`. `\\R` treats
   CRLF as one consuming unit in both capture-aware and regex-set execution.

## Explicit non-goals

See [Compatibility](../reference/compatibility.md) for the split between the
regular NFA path and the bounded advanced executor. The project does not embed
Perl-style code interpolation or fuzzy matching.

## Known gaps

- **Coverage sits below 100%/100%, so the Nix check gates at 96%/92%
  instead** (96.49% expression / 94.23% branch as of this writing, up from
  95.63%/93.38% after a pass that downloaded the `sb-cover` HTML report
  from CI and read every uncovered line across all 23 flagged files,
  rather than assuming the whole gap was structural). That pass separated
  two real categories:
  - Structural `sb-cover` blind spots that account for most of the
    remaining 252 uncovered expressions: `in-package`, value-less
    `defvar`/`defconstant`, `defmacro`/`defclass` bodies, `defparameter`
    data literals, and `&key` defaults never show "executed" regardless of
    how thoroughly the surrounding file is exercised -- confirmed by
    `scan`'s `(start 0)` default still showing uncovered even though
    several different test files already call it without `:start`. These
    cannot be closed by adding tests; `checks.coverage`'s 96%/92% gate
    (`flake.nix`) accepts this permanently rather than blocking CI on an
    unreachable number.
  - Real, reachable logic nothing exercised, closed in that same pass (see
    the `git log` message "close real coverage gaps found by inspecting
    the sb-cover HTML report" for the full list): a character-class item
    ordering that flushes pending literal ranges before a POSIX class or
    an escape matcher, `\P{...}` negation and `\B`/`\C`/`\Q`-as-literal
    inside a class, the lazy optional `a??` quantifier, an alternation
    whose first (not second) branch is itself non-static, `\p{NChar}`'s
    fast-path range check, and a Unicode property
    (`Grapheme_Extend`/U+09BE) that falls outside SBCL's own grapheme-break
    classification.
  - Not conclusively resolved: two defensive "should never happen" `error`
    catch-alls in `nfa.lisp` (every `inst-op` and AST node subtype the
    compiler can emit already has its own case arm, so these look like
    closed-enumeration guards rather than reachable gaps -- kept for
    defensiveness against a future unhandled case rather than deleted to
    chase the coverage number); one arm of `changes-when-case-mapped-p`'s
    lowercase/titlecase/uppercase `or` chain in
    `unicode-property-resolver.lisp` (titlecase and uppercase mappings
    coincide for ordinary letters, so the short-circuit order makes the
    uppercase-only arm hard to isolate without a character where titlecase
    is the identity but uppercase is not); three `fail` branches in
    `regex-grammar-support.lisp`/`regex-grammar.lisp` whose triggering
    input wasn't found through code reading alone. `ensure-byte-character`'s
    own `> #xff` check is likely genuinely unreachable: every call site
    that reaches it (`ensure-byte-class-character`, `range`) either already
    rejects a non-raw-octet character above `#x7f` first, or only ever
    passes bounds that are structurally `<= #xff` (`\xHH` and `\ooo`
    escapes cannot produce a larger value).

## Future extensions

- Broader PCRE-compatible syntax beyond the current advanced executor, with
  each addition requiring parser, AST, runtime, replacement, regex-set, and
  semantic regression coverage
- Unicode conformance expansion for grapheme, word, and sentence boundary
  behavior, including more UAX-29 edge-case fixtures
- Complete the normal ASDF load path for the advanced executor and keep the
  standard Nix test entry point green
