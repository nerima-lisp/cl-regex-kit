# Roadmap

## Current status

The scaffold is in place: package, conditions, and the full `regex-node` AST
hierarchy are real. `parse-regex`, `compile-to-nfa`, and `run-pike-vm` are
stubs that signal an error until implemented.

## Planned, in dependency order

1. **`parse-regex`** -- literals, concatenation, alternation, grouping
   (capturing and `(?:...)`), repetition (`*`, `+`, `?`, `{m,n}`, greedy and
   lazy), character classes (including negation and the `\d`/`\w`/`\s`
   shorthands), `.`, and anchors (`^`, `$`).
2. **`compile-to-nfa`** -- Thompson construction from the AST to the `inst`
   program, one case per `regex-node` subclass.
3. **`run-pike-vm`** -- the thread-set simulation itself: sparse-set thread
   deduplication, capture-slot save/restore per thread, and leftmost-first
   priority at `:split`.
4. **`all-matches`** -- repeated `scan` calls advancing past each match.
5. Property-based tests for `parse-regex` (never signals anything but
   `regex-syntax-error`) and round-trip tests once there is a serializer to
   round-trip against, per the org's
   [test standard](https://github.com/nerima-lisp/.github/blob/main/TEST_STANDARD.md#property-based-テスト).

## Explicit non-goals

See [Compatibility](compatibility.md): backreferences and unbounded lookaround
are not planned, by design.
