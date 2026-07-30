# Compatibility

`cl-regex-kit` is built on Thompson NFA construction and Pike's VM simulation,
the same foundation as RE2 and Rust's `regex` crate. That foundation is what
guarantees matching time linear in the input -- and it is also what rules out
a few features every backtracking-based engine (Perl, PCRE, Python's `re`)
supports.

## Implemented RE2/Rust-style syntax

- Literals, alternation, capturing and non-capturing groups
- Named captures: `(?<name>...)` and `(?P<name>...)`; capture names work with
  `match-group-*` and `regex-group-index`
- Greedy and lazy `*`, `+`, `?`, and `{m,n}` repetition (finite bounds up to
  1000)
- Character classes, ranges, POSIX classes, and intersection (`&&`) or
  difference (`--`) set operations
- Unicode-aware `\\d`/`\\w`/`\\s` shorthands, `\\p{...}`/`\\P{...}` Unicode
  general-category, Script, Block, Age, and selected binary properties, and
  `\\b`/`\\B` boundaries, Rust-style `\\b{start}`/`\\b{end}`/`\\b{start-half}`/`\\b{end-half}` boundary variants,
  and RE2-style `\\<`/`\\>` word boundaries
- `\\a`, `\\f`, `\\n`, `\\r`, `\\t`, `\\v`, `\\ooo`, `\\xHH`, `\\x{...}`, `\\uHHHH`,
  and `\\u{...}` escapes, plus RE2-style `\\Q...\\E` quoted literals
- `.` and inline `i`, `m`, `s`, `R`, `U`, `x`, and `u` flags, including scoped and disabling
  forms such as `(?im-s:...)`
- `^`/`$` and absolute `\\A`/`\\z` anchors
- Leftmost-first scanning, non-overlapping iteration, bounded splitting, and
  first/all replacement with Rust-style capture templates (`$0`, `$name`,
  `${name}`, and `$$`)
- RE2/Rust-style multi-pattern matching through `compile-regex-set`,
  `regex-set-matches`, and `regex-set-match-p`; duplicate patterns retain their
  individual source indexes

## Not supported, by design

### Backreferences (`\1`, `\2`, ...)

Matching `\1` requires comparing the input against text captured *at match
time* -- a context-sensitive requirement that a finite automaton cannot
express. Supporting it means falling back to backtracking for the whole
pattern, which reintroduces the exponential worst case this engine exists to
avoid.

### Unbounded lookaround

Fixed-width lookahead/lookbehind can sometimes be compiled into the automaton,
but general, unbounded lookaround has the same fundamental issue as
backreferences: it asks the engine to reason about text outside what a
left-to-right automaton walk has available.

## Why this trade

RE2's own documentation states this trade explicitly, and this project makes
the same choice: a smaller, well-defined feature set in exchange for a
worst-case time guarantee that holds for *any* input, including adversarial
ones. An application that must accept untrusted patterns or untrusted input
text benefits from this guarantee in a way that a backtracking engine,
however featureful, cannot provide.

If a project needs backreferences or unbounded lookaround, a backtracking
engine such as [cl-ppcre](https://edicl.github.io/cl-ppcre/) is the
appropriate tool -- and not a defect in either design, just a different point
on the same trade-off.

## Current differences

- Unicode property support is backed by SBCL's Unicode data. Script aliases
  such as `\\p{Greek}`, `Script=Greek`, `Block=Greek_And_Coptic`, and
  `Age=V15_0` are supported; Script_Extensions is not currently distinct from
  Script.
- The project currently targets SBCL; portability across Common Lisp
  implementations has not been established.
- To retain predictable compilation resources, a compiled NFA program is
  limited to 100000 instructions. Patterns exceeding this limit signal
  `regex-syntax-error` rather than allocating without bound.
- `regex-set` preserves the individual linear-time guarantees of its compiled
  members. It currently evaluates members independently, so a set match takes
  `O(pattern-count * text-length)` time rather than using a fused set automaton.

See the [Roadmap](roadmap.md) for planned extensions.
