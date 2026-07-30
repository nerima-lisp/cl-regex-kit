# Architecture

## File layout

```text
src/
  package.lisp    the single defpackage
  conditions.lisp cl-regex-kit-error and its subtypes
  ast.lisp        REGEX-NODE and its subclasses -- the parser's output shape
  parser.lisp     PARSE-REGEX: pattern string -> REGEX-NODE tree
  nfa.lisp        COMPILE-TO-NFA: REGEX-NODE tree -> INST program (Thompson construction)
  pike-vm.lisp    RUN-PIKE-VM: INST program -> MATCH-RESULT (thread simulation)
  api.lisp        the public entry points wiring the three stages together
```

`src/` is flat, per the org's [package
standard](https://github.com/nerima-lisp/.github/blob/main/PACKAGE_STANDARD.md#リポジトリ直下の構成):
the `.asd` `:components` list is the table of contents, and every public
symbol lives in `src/package.lisp`.

## Data flow

See [Core concepts](concepts.md) for the full explanation of each stage. In
one line: `parser.lisp` and `nfa.lisp` are pure compilation (pattern in,
program out, or a `regex-syntax-error`); `pike-vm.lisp` is where matching
actually happens, and it is the only stage that runs once per call to `scan`
rather than once per call to `compile-regex`.

## Why compilation and matching are split

`compile-regex` does the expensive, pattern-dependent work (parsing, Thompson
construction) exactly once. `scan` only walks the resulting `inst` program
against the input, so matching the same pattern against many strings pays the
compilation cost once instead of once per call -- this is why `match` is
documented as a convenience wrapper and `compile-regex` + `scan` is the
recommended path for repeated matching.

## Condition hierarchy

Every condition this library signals derives from `cl-regex-kit-error`, so a
caller can catch every failure this library can produce with one
`handler-case` clause:

```lisp
(handler-case (compile-regex user-supplied-pattern)
  (cl-regex-kit-error (c)
    (report-bad-pattern c)))
```

`regex-syntax-error` is the only subtype so far; it carries the offending
pattern, the parser's best guess at the failing position, and a human-readable
reason.
