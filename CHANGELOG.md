# Changelog

All notable changes to this project are documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [0.2.0] - 2026-07-31

### Added

- Add `cl-regex-kit-grep` (`cli/`), a small `grep`-alike built directly on
  the [`cl-cli`](https://github.com/nerima-lisp/cl-cli) declarative app
  builder (`make-app`/`make-option`/`make-positional`/`run-app`), with no
  adapter layer. It is its own `cl-regex-kit/cli` `.asd` system
  (`:depends-on ("cl-regex-kit" "cl-cli")`, `:build-operation "program-op"`)
  so `cl-regex-kit` itself keeps its `("cl-parser-kit")`-only dependency
  list, and `flake.nix` builds it as `packages.cl-regex-kit-grep` via
  `cl.mkExecutable`. Supports `-i`/`--ignore-case`, `-v`/`--invert-match`,
  `-c`/`--count`, `-n`/`--line-number`, reading from files or standard
  input, with grep-compatible exit codes.

### Changed

- Merge `replace-up-to`'s duplicated byte/string `DO-MATCHES` accumulation
  loop into `accumulate-replacements`, taking an `append` continuation for
  the one thing the two domains do differently (push octets onto an
  adjustable vector, or write a string onto a stream).
- Extend the `t/parser-test.lisp` `it-each` conversion to
  `t/api-properties-test.lisp`'s Unicode binary-property match/non-match
  tables (40 rows).
- Rewrite `t/parser-test.lisp`'s table-driven cases (RE2/Rust escape forms,
  octal escapes, POSIX classes, malformed patterns) from a single `it` with
  an internal `dolist` -- one pass/fail result for the whole table -- onto
  `cl-weave`'s `it-each`, which expands each row into its own named,
  independently reported test case (~40 rows across four tables become 168
  individually diagnosable tests instead of 4).
- Remove `regex-tokenizer-escapes.lisp`'s `scan-braced-hex-code` (dead after
  the `\x{...}`/`\u{...}`/`\U{...}` extended-mode fix below moved braced-hex
  scanning to the grammar layer, leaving this tokenizer-level version with
  no remaining callers) and the now-unreachable `braced-p` branches inside
  `scan-escaped-character`. Remove the redundant `unicode-property-name-p`
  re-check in `build-escape-atom`/`class-item-from-escape`
  (regex-grammar.lisp/regex-grammar-classes.lisp): `\p{...}`/`\P{...}`
  escape names are already validated, unconditionally, by
  `scan-unicode-property-name` at tokenize time, so a name reaching the
  grammar layer is always already known-valid. Coverage rose from
  94.38%/92.44% to 95.02%/93.87% partly as a direct result -- these were
  genuinely unreachable branches, not undertested ones.
- Merge `compile-regex`/`compile-byte-regex`'s ~90%-duplicated bodies into
  one shared `%compile-pattern`, and have `validate-regex-compile-options`
  return the parser flags it already computes for validation instead of
  each caller recomputing them a second time -- previously the validation
  call and the "real" call built the flags independently, a latent hazard
  if a future edit changed one and not the other.
- Merge `expand-replacement-template`/`expand-byte-replacement-template`'s
  duplicated `$name`/`${name}` scanner into one
  `expand-replacement-template-generic`, parameterized over the
  character/octet domain's delimiter values and emit functions.
- Split `nfa.lisp`'s `compile-to-nfa` (a single 117-line function closing
  over a 13-function `labels` block) into ordinary top-level `defun`s over
  dynamically-bound compiler state (`*nfa-instructions*` etc.), the same
  technique the parser already uses for its own shared state.
- Rewrite the parser onto `cl-parser-kit`'s token/span model: a new
  `regex-tokenizer.lisp`/`regex-tokenizer-escapes.lisp` turn a pattern string
  into a `(vector cl-parser-kit:token)` in one forward pass, and
  `regex-grammar.lisp`/`regex-grammar-classes.lisp` replace the old
  `parser.lisp`/`parser-escapes.lisp`/`parser-classes.lisp` character
  scanner with hand-written recursive descent over that token vector.
  `cl-parser-kit` becomes `cl-regex-kit`'s first real (non-test-only)
  dependency (`cl-regex-kit.asd`'s `:depends-on`, `flake.nix`'s
  `lispDependencies`, pinned to `v1.0.1`). See
  [Architecture](https://nerima-lisp.github.io/cl-regex-kit/architecture/#from-a-character-scanner-to-a-cl-parser-kit-token-stream)
  for why the grammar tiers stay hand-written rather than built on
  `cl-parser-kit`'s combinator/Pratt layer, and for the two escape forms
  (`\x{...}`/`\u{...}`/`\U{...}`, `\b{...}`) whose extended-mode whitespace
  handling had to move from the tokenizer to the grammar layer as a result.
- Unify `run-pike-vm`/`run-pike-vm-set`'s duplicated ~130-line epsilon-
  closure walk into one `pike-vm-closure` function taking an optional
  `on-save` callback, so set matching's uncaptured threads and capturing
  matches' slot bookkeeping no longer require two copies of the
  `:split`/`:jmp`/`:save`/zero-width traversal.
- Generate `character-class.lisp`'s word-boundary/-start/-end/-start-half/
  -end-half predicate family for all three domains (ASCII bytes, Unicode-
  aware bytes, strings) from one `define-boundary-predicates` macro and a
  per-domain "who is a word character here" primitive, instead of fifteen
  hand-written near-identical functions (plus a further copy inlined in
  `pike-vm.lisp`).
- Split `unicode-properties.lisp` (869 lines, roughly 70% embedded UCD
  literal data) into pure logic and a new `unicode-binary-property-range-
  data.lisp` holding every binary-property name list, value-alias table,
  and code-point range table it looks up by name.
- Replace the repetitive `regex-node` subclass `defclass` forms with a
  `define-regex-node` macro that generates each node's slots from a compact
  spec, keeping node shape as data and generation as shared logic.
- Unify the character- and byte-domain character-class matcher evaluators
  (`matcher-matches-p`, `byte-matcher-matches-p`) behind one
  `define-set-matcher` macro, removing duplicated set-operator dispatch.
- Share compile-time argument validation across the `regex`, `byte-regex`,
  `regex-set`, and `byte-regex-set` literal-compiling macros instead of
  repeating it in each one.
- Bump the `cl-weave` test dependency to v1.1.0.
- Add property-based tests covering UTF-8 round-tripping, NFA compilation,
  byte-mode parsing, and VM match bounds.
- Rebuild `flake.nix` on `cl-nix-forge`'s `mkPackageFlake` preset instead of
  hand-written derivations, keeping only the project-specific coverage
  percentage-threshold gate as a custom check.
- Split the 631-line `parser.lisp` into `parser.lisp` (shared state and core
  grammar), `parser-escapes.lisp`, and `parser-classes.lisp` by converting
  `parse-regex`'s closure-captured state into dynamically-bound special
  variables, so its ~49 parsing functions can live as top-level `defun`s
  across files instead of nested inside one `labels` form.
- Factor the validate-then-run-under-timeout shape shared by `scan`,
  `shortest-match`, `longest-match`, `regex-set-matches-into`, and
  `regex-set-match-p` into `call-with-validated-match` /
  `call-with-validated-regex-set-match`, continuation-passing helpers that
  take a thunk of the validated match limit.

## [0.1.0] - 2026-07-30

### Fixed

- Make non-overlapping iteration suppress an empty match immediately after a
  non-empty match, matching Rust `Regex` iteration, splitting, and replacement
  semantics.
- Reject a single string passed as a regex-set pattern container instead of
  treating its characters as individual malformed patterns.
- Validate replacement values and split inputs even when a replacement or split
  is skipped by an empty limit or a non-matching pattern.

### Added

- Add Rust `Regex::split_inclusive`-compatible delimiter-retaining splitting
  through `split-inclusive` for character and byte regexes.
- Add RE2/Rust-compatible one- through three-digit octal escapes, rejecting
  values outside the byte range.
- Add empty character classes: `[]` matches no input, while `[^]` matches any
  alphabet element, including for byte regexes and character-class set
  operations.
- Allow byte regex and byte regex set builders to use any octet as
  `:line-terminator`, including non-ASCII values.
- Complete the Thompson NFA and Pike VM implementation for RE2/Rust-compatible
  linear-time matching, including capture, byte-regex, replacement, and
  regex-set APIs.
