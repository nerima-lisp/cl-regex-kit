# Core concepts

`cl-regex-kit` compiles a pattern in three stages, mirroring the design Russell
Cox documents in
["Regular Expression Matching Can Be Simple and Fast"](https://swtch.com/~rsc/regexp/regexp1.html)
and its sequels, and the one RE2 and Rust's `regex` crate ship in production.

```text
pattern string --[regex-tokenizer.lisp]--> token vector --[regex-grammar.lisp]--> REGEX-NODE tree --[nfa.lisp]--> INST program --[pike-vm.lisp]--> MATCH-RESULT
```

## 1. Parsing (`src/regex-tokenizer.lisp`, `src/regex-grammar.lisp`, `src/ast.lisp`)

`parse-regex` first tokenizes the pattern into a `(vector cl-parser-kit:token)`
(`regex-tokenizer.lisp`), then runs a recursive-descent parser over that token
vector (`regex-grammar.lisp`/`regex-grammar-classes.lisp`): alternation over
concatenations, concatenation over repeated atoms, an atom is a literal, a
group, a character class, `.`, or an anchor. It produces a tree of
`regex-node` subclasses (`ast.lisp`) -- one class per syntax feature, with no
separate "optimized" tree, since the next stage compiles this shape directly.

## 2. Thompson construction (`src/nfa.lisp`)

`compile-to-nfa` walks the AST and emits a flat program: a vector of `inst`
values (`:char`, `:class`, `:any`, `:split`, `:jmp`, `:save`, `:match`). This is
Thompson's construction -- each node type expands to a small, fixed sequence of
instructions with epsilon-like control flow (`:split`, `:jmp`), so program size
stays linear in pattern size.

`:split` takes two targets and tries the first before the second, which is how
greedy-vs-lazy repetition and leftmost alternation encode their priority.

## 3. Pike's VM (`src/pike-vm.lisp`)

`run-pike-vm` executes the program against the input **without ever
backtracking**. Instead of following one path at a time and retrying on
failure, it advances a deduplicated *set* of active threads one input
character at a time:

1. Compute the epsilon-closure of the current thread set (follow every
   `:split`/`:jmp`/`:save` reachable without consuming input).
2. Deduplicate by program-counter, keeping only the highest-priority thread at
   each PC -- this is what bounds the thread count by program size and keeps
   matching linear in the input.
3. Step every surviving thread against the current input character.
4. Repeat until input is exhausted or every thread has died.

Because a failed thread is simply dropped from the set rather than retried,
there is no path that gets explored twice -- the mechanism that gives
backtracking engines their exponential worst case does not exist here.

### Captures without backtracking

Plain NFA/DFA simulation only tracks *which states are reachable*, not *how
they were reached*, so it cannot recover capture-group boundaries on its own.
Pike's VM fixes this by giving each thread its own capture-slot vector: a
`:save` instruction records the current input offset into that thread's slots.
When two threads reach the same program counter, only the higher-priority one
survives (per step 2 above) -- and priority order was assigned by `:split` to
match leftmost-first, greedy semantics -- so the recorded slots are always
consistent with what a backtracking engine would have found by exploring in
priority order.

## Why this instead of backtracking

A textbook backtracking engine (and most engines you meet day to day: Perl,
PCRE, Python's `re`) explores one path at a time and retries on failure. On
adversarial patterns like `a?a?a?...aaa` against a run of `a`s, that degrades
to exponential time, because failed combinations get re-explored instead of
merged away. The thread-set simulation above never repeats work, so matching
time is `O(pattern-size * input-size)` regardless of the input.

The trade is expressiveness: [backreferences and unbounded
lookaround](../reference/compatibility.md) require matching against runtime-captured text
or unbounded lookahead, which a finite automaton cannot represent without
giving up the linear-time guarantee. `cl-regex-kit` chooses the guarantee.
