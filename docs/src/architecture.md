# Architecture

## File layout

```text
src/
  package.lisp    the single defpackage
  conditions.lisp cl-regex-kit-error and its subtypes
  ast.lisp        REGEX-NODE and its subclasses -- the parser's output shape
  unicode-property-data.lisp
                  Unicode property aliases and static range tables
  unicode-age-data.lisp
                  Generated Unicode Age range table
  unicode-properties.lisp
                  Unicode property normalization and matching predicates
  unicode-case-folding-data.lisp
                  Generated Unicode simple case-folding table
  character-class.lisp
                  class composition, case folding, and boundary predicates
  parser-syntax.lisp
                  parser flags, capture-name grammar, and option normalization
  parser.lisp     PARSE-REGEX: stateful pattern parsing -> REGEX-NODE tree
  nfa.lisp        COMPILE-TO-NFA: REGEX-NODE tree -> INST program (Thompson construction)
  pike-vm.lisp    RUN-PIKE-VM: INST program -> MATCH-RESULT (thread simulation)
  api.lisp        compiled-regex model, compilation, literal macros, metadata
  api-match.lisp  input validation, timeout handling, scans, and match accessors
  api-operations.lisp
                  non-overlapping iteration and split operations
  api-replace.lisp
                  replacement templates and replacement operations
  regex-set.lisp  multi-pattern compilation and matching
```

`src/` is flat, per the org's [package
standard](https://github.com/nerima-lisp/.github/blob/main/PACKAGE_STANDARD.md#リポジトリ直下の構成):
the `.asd` `:components` list is the table of contents, and every public
symbol lives in `src/package.lisp`.

The API modules follow the execution direction rather than grouping unrelated
public functions in one file: `api.lisp` creates immutable compiled values,
`api-match.lisp` executes one match, `api-operations.lisp` consumes the match
stream for iteration and splitting, and `api-replace.lisp` adds replacement
template expansion.  This keeps matching independent of higher-level
operations and makes the ASDF serial order the dependency order.

## Data flow

See [Core concepts](concepts.md) for the full explanation of each stage. In
one line: `parser-syntax.lisp`, `parser.lisp`, and `nfa.lisp` are pure
compilation (pattern in, program out, or a `regex-syntax-error`);
`parser-syntax.lisp` owns context-free parser rules and translates public
options into flags, while `parser.lisp` owns the stateful recursive descent.
`unicode-property-data.lisp` owns Unicode aliases and static range data.
`unicode-age-data.lisp` owns the generated Unicode Age range table, while
`unicode-properties.lisp` performs name resolution and range matching.
Range-defined binary properties
are declared as alias-to-range entries and dispatched through one shared
matcher, keeping property additions reviewable without duplicating control
flow. `unicode-case-folding-data.lisp` owns the generated Unicode simple
case-folding table. `character-class.lisp` consumes that data for class
composition, case folding, and boundary predicates. `pike-vm.lisp` executes
matching and is the only stage that runs once per call to `scan` rather than
once per call to `compile-regex`.

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
