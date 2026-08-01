# cl-regex-kit

`cl-regex-kit` is a **from-scratch regular expression engine for Common
Lisp**. It compiles a pattern to an AST, then to a Thompson-constructed NFA
program, and matches it with a Pike's VM thread simulation -- the architecture
used by [RE2](https://github.com/google/re2) and
[Rust's `regex` crate](https://github.com/rust-lang/regex) -- so matching time
stays linear in the input length regardless of the pattern.

Everything is portable Common Lisp with no runtime dependencies; only the test
system uses [cl-weave](https://github.com/nerima-lisp/cl-weave).

Start with [Getting started](getting-started.md), then read [Core
concepts](guide/concepts.md) for the parser -> NFA -> Pike's VM pipeline and
[Compatibility](reference/compatibility.md) for what this engine deliberately does not
support.

<div class="grid cards" markdown>

-   :material-rocket-launch: **Get started**

    ---

    Load the system and compile your first pattern.

    [:octicons-arrow-right-24: Getting started](getting-started.md)

-   :material-sitemap: **Learn the pipeline**

    ---

    Why an NFA simulation instead of backtracking, and how captures survive it.

    [:octicons-arrow-right-24: Core concepts](guide/concepts.md)

-   :material-format-list-bulleted: **Look something up**

    ---

    Every exported symbol with its signature and return values.

    [:octicons-arrow-right-24: API Reference](reference/api.md)

-   :material-alert-circle-outline: **Check compatibility**

    ---

    What a finite automaton cannot express, and why that's a deliberate trade.

    [:octicons-arrow-right-24: Compatibility](reference/compatibility.md)

</div>

## Why another regex engine?

Most regex engines you meet day to day -- Perl's, PCRE, Python's `re` -- are
backtracking interpreters: fully general, but capable of exponential blowup on
adversarial patterns (`a?a?a?...aaa` against a run of `a`s is the classic
case). RE2 and Rust's `regex` took a different approach: compile the pattern
to a nondeterministic finite automaton and simulate every reachable state at
once, character by character, so a failed path is merged away instead of
retried. That structurally forecloses catastrophic backtracking, at the cost
of a smaller feature set -- no backreferences, no lookaround.

`cl-regex-kit` follows that second design, from scratch, in Common Lisp.

## Design notes

- **No backtracking, ever.** Matching advances a deduplicated set of active
  NFA threads; a path that fails is dropped from the set, never retried.
- **Captures without giving up the guarantee.** Each thread carries its own
  capture-slot vector (Pike's VM), so leftmost-first submatch semantics survive
  the simulation instead of requiring a return to backtracking.
- **A deliberately smaller feature set.** Backreferences and lookaround are
  out of scope from the start -- see
  [Compatibility](reference/compatibility.md) -- because retrofitting them later would
  mean reintroducing the exponential blowup the whole design avoids.
