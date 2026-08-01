# Architecture

## File layout

```text
src/
  package.lisp    the single defpackage
  conditions.lisp cl-regex-kit-error and its subtypes
  ast.lisp        REGEX-NODE and its subclasses -- the parser's output shape
  unicode-property-data.lisp
                  Unicode property aliases and static range tables
  unicode-extra-binary-property-data.lisp
                  Generated regex-syntax UCD property_bool.rs range table
  unicode-age-data.lisp
                  Generated Unicode Age range table
  unicode-binary-property-range-data.lisp
                  Unicode binary-property name lists, value aliases, and
                  code-point range tables (DECODE-CODE-POINT-RANGES and
                  every table it decodes)
  unicode-properties.lisp
                  Unicode property normalization and matching predicates,
                  as pure logic over the tables above
  unicode-case-folding-data.lisp
                  Generated Unicode simple case-folding table
  character-class.lisp
                  class composition, case folding, and boundary predicates
  parser-syntax.lisp
                  parser flags, capture-name grammar, option normalization, and
                  the shared parser-state special variables
  regex-tokenizer-escapes.lisp
                  lexical decoding of hex/octal/Unicode-property/word-boundary
                  escape bodies, shared by both tokenizer dispatch tables
  regex-tokenizer.lisp
                  TOKENIZE-REGEX-PATTERN: pattern string -> (VECTOR
                  cl-parser-kit:token), tracking only character-class nesting
  regex-grammar.lisp
                  PARSE-REGEX and the core grammar over that token vector:
                  alternation, concatenation, quantifiers, groups, inline flags
  regex-grammar-classes.lisp
                  character-class body grammar: `[...]`, POSIX classes, set
                  operators, over the same token vector
  nfa.lisp        COMPILE-TO-NFA: REGEX-NODE tree -> INST program (Thompson construction)
  pike-vm.lisp    RUN-PIKE-VM: INST program -> MATCH-RESULT (thread simulation)
  api.lisp        compiled-regex model, compilation, literal macros, metadata
  api-match.lisp  input validation, timeout handling, scans, and match accessors
  api-operations.lisp
                  non-overlapping iteration and split operations
  api-replace.lisp
                  replacement templates and replacement operations
  regex-set.lisp  multi-pattern compilation and matching
cli/
  package.lisp    the CL-REGEX-KIT/CLI package, importing COMPILE-REGEX/
                  IS-MATCH-P from cl-regex-kit and cl-cli's app-builder API
  main.lisp       cl-regex-kit-grep: a grep-alike built directly on cl-cli's
                  MAKE-APP/MAKE-OPTION/MAKE-POSITIONAL/RUN-APP, with no
                  adapter layer
```

`src/` is flat, per the org's [package
standard](https://github.com/nerima-lisp/.github/blob/main/PACKAGE_STANDARD.md#リポジトリ直下の構成):
the `.asd` `:components` list is the table of contents, and every public
symbol lives in `src/package.lisp`. `cli/` is a second, equally flat
component tree for the `cl-regex-kit/cli` system (`cl-regex-kit.asd`), which
builds the `cl-regex-kit-grep` executable rather than a library.

The API modules follow the execution direction rather than grouping unrelated
public functions in one file: `api.lisp` creates immutable compiled values,
`api-match.lisp` executes one match, `api-operations.lisp` consumes the match
stream for iteration and splitting, and `api-replace.lisp` adds replacement
template expansion.  This keeps matching independent of higher-level
operations and makes the ASDF serial order the dependency order.

## Data flow

See [Core concepts](../guide/concepts.md) for the full explanation of each stage. In
one line: `parser-syntax.lisp`, `regex-tokenizer*.lisp`,
`regex-grammar*.lisp`, and `nfa.lisp` are pure compilation (pattern in,
program out, or a `regex-syntax-error`); `parser-syntax.lisp` owns
context-free parser rules, the shared parser-state special variables, and
translates public options into flags. `regex-tokenizer.lisp` turns the
pattern string into a token vector in one forward pass -- the only context it
tracks is character-class nesting, since that is the one place this
grammar's lexical rules genuinely depend on position (`-`, `]`, `^`, POSIX
classes, and set operators are meaningful only inside `[...]`, and `\b`
denotes a backspace character there but a word boundary outside one).
Everything else the grammar is context-sensitive about -- the live Unicode
flag toggled by inline `(?u)`/`(?-u)`, byte-mode legality, extended-mode
whitespace -- depends on *parser state reached along a particular parse
path*, not lexical position, so the tokenizer defers it: every escape token
carries a fully-decoded but unvalidated shape, and `regex-grammar.lisp`/
`regex-grammar-classes.lisp` apply flag-dependent legality checks and build
the `regex-node` tree while those flags are actually in scope. Both grammar
files are hand-written recursive descent over the token vector rather than
`cl-parser-kit` combinator pipelines, because this grammar has no genuine
backtracking ambiguity -- every branch point resolves on one token of
lookahead -- which is also why `cl-parser-kit`'s tokenizer-rule and
Pratt/expression-parser layers are not used: see [Packages evaluated from the
`nerima-lisp` org](#packages-evaluated-from-the-nerima-lisp-org) below for
what `cl-parser-kit` does contribute.
`unicode-property-data.lisp`, `unicode-extra-binary-property-data.lisp`,
`unicode-age-data.lisp`, and `unicode-binary-property-range-data.lisp` own
every Unicode alias list and static range table this engine consults;
`unicode-properties.lisp` performs name resolution and range matching as
pure logic over those tables, with no embedded literal data of its own.
Range-defined binary properties are declared as alias-to-range entries
(`DEFINE-UNICODE-RANGE-PROPERTIES`) and dispatched through one shared
matcher, keeping property additions reviewable without duplicating control
flow. `unicode-case-folding-data.lisp` owns the generated Unicode simple
case-folding table. `character-class.lisp` consumes that data for class
composition, case folding, and boundary predicates -- its own five-shape
boundary/start/end/start-half/end-half algebra (word boundaries in ASCII
byte, Unicode-aware byte, and string domains) is generated once by
`DEFINE-BOUNDARY-PREDICATES` from a per-domain "who is a word character
here" primitive, rather than written out three times. `pike-vm.lisp` executes
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

## Continuation-passing validation

`scan`, `shortest-match`, and `longest-match` (`api-match.lisp`) -- and
`regex-set-matches-into` and `regex-set-match-p` (`regex-set.lisp`) -- all
follow the same shape: validate the regex and the input range, then run
`run-pike-vm`/`run-pike-vm-set` under a timeout with slightly different
keyword arguments. `call-with-validated-match` and
`call-with-validated-regex-set-match` factor that shape out in
continuation-passing style: each takes a `thunk` of one argument (the
validated `limit`) and is responsible only for validation and the timeout
wrapper, while the thunk supplies the one thing that differs per caller --
which VM entry point to call and with which flags. `do-matches`/`do-captures`
(`api-operations.lisp`) and `call-with-timeout` (`api-match.lisp`) already used
this shape (a macro body, or a caller-supplied closure, run under a
controlling function); these two additions extend it to the validate-then-run
path shared by every one-shot match entry point. `pike-vm.lisp`'s
`pike-vm-closure` is the same idea applied to `run-pike-vm`/`run-pike-vm-set`'s
epsilon-closure walk: the ~130-line `:split`/`:jmp`/`:save`/zero-width
traversal was duplicated once per entry point, differing only in whether
`:save` updates a capture-slot vector; factoring it into one function taking
an optional `on-save` callback removes the duplication without forcing
set-matching's uncaptured threads through the same slot bookkeeping
capturing matches needs.

## Macro-centric design and its limits

Where a shape repeats across several definitions, the repetition is factored
into one macro instead of being copied: `define-regex-node` (`ast.lisp`)
generates every `regex-node` subclass from a compact slot spec,
`define-set-matcher` (`character-class.lisp`) generates both the character-
and byte-domain matcher evaluators from one set of leaf clauses,
`define-boundary-predicates` (`character-class.lisp`) generates the
boundary/start/end/start-half/end-half predicate family for each of the
three word-boundary domains from one "who is a word character here"
primitive, `define-unicode-range-properties` (`unicode-properties.lisp`)
builds the alias-to-range table every range-defined binary Unicode property
is dispatched through, and the `regex`/`byte-regex`/`regex-set`/
`byte-regex-set` literal-compiling macros share one compile-time argument
validator. In each case the macro is pure compile-time code generation over
data the call site supplies -- nothing here hides runtime control flow
behind a macro.

The parser's shared state -- position in the token stream, accumulated
flags, capture bookkeeping -- lives in the dynamically-bound special
variables `parser-syntax.lisp` declares (`*regex-token-position*`,
`*regex-flags*`, and so on), bound once per `parse-regex` call via `let*` in
`regex-grammar.lisp`; every grammar function reads and mutates them
directly. This is the same technique CL-PPCRE's own recursive-descent parser
uses, carried over unchanged from before the tokenizer rewrite below, and it
is what lets the grammar be ordinary top-level `defun`s split across
`regex-grammar.lisp` (shared state and core grammar) and
`regex-grammar-classes.lisp` (character classes) rather than one form, with
the same per-call isolation (each `parse-regex` invocation gets its own
dynamic extent, so concurrent calls from different threads never share a
binding).

## From a character scanner to a `cl-parser-kit` token stream

Through mid-2026 the parser scanned `*regex-pattern*` one character at a
time (`peek`/`take`, `parser.lisp`/`parser-escapes.lisp`/
`parser-classes.lisp`), including its own hand-rolled extended-mode
whitespace/comment skipping. It has since been rewritten onto
`cl-parser-kit`'s token/span model: `regex-tokenizer.lisp` turns the pattern
into a `(vector cl-parser-kit:token)` in one forward pass, and
`regex-grammar.lisp`/`regex-grammar-classes.lisp` consume that vector instead
of the raw string. The escape/hex/octal/Unicode-property/POSIX-class
character-scanning that used to be interleaved with grammar decisions across
all three old files now lives in one place, `regex-tokenizer-escapes.lisp`,
decoding each escape into an unvalidated `(:kind ... payload...)` shape
before the grammar ever sees it.

`cl-parser-kit` also ships a token-stream parser-combinator layer
(`seq`/`alt`/`many`/`sep-by1`/Pratt) that this parser deliberately does not
build the grammar tiers on. Every branch point in RE2/Rust regex
syntax resolves on one token of lookahead -- there is no construct where the
parser must try one alternative, fail, and backtrack to another -- so the
combinator layer's backtracking-failure machinery would add a layer of
`parse-failure`/diagnostic plumbing this grammar has no use for, in exchange
for none of the precedence-climbing power it exists to provide (regex has
exactly one real infix operator, `|`, at one fixed precedence). The
tokenizer/token/span data model is the part of `cl-parser-kit` this grammar
actually needed.

Two pieces of the original character-level design could not survive the
move unchanged, both because `cl-parser-kit` tokenizes the whole pattern in
one flag-independent pass before any parsing happens, while the original
scanner's behavior at a few points depended on the *live*, inline-mutable
extended-mode flag at that exact position in the parse:

- **`\x{...}`/`\u{...}`/`\U{...}` and `\b{...}`.** The original
  `PARSE-BRACED-HEX-CODE`/`PARSE-WORD-BOUNDARY` read their digit/name run
  through the same `peek`-based cursor as everything else, so extended mode
  tolerates whitespace inside the braces (`(?x)\x{ 6 1 }` parses as `0x61`).
  The tokenizer cannot evaluate that live flag, so it stops at the opening
  brace (emitting a `:HEX-BRACE-OPEN` token, or a bare word-boundary escape
  followed by an ordinary `:LBRACE` token) and leaves the digit/name
  collection to `COLLECT-BRACED-HEX`/`PARSE-WORD-BOUNDARY-SUFFIX` in
  `regex-grammar.lisp`, which read the token stream with the grammar's own
  extended-mode-aware significant-token cursor -- the one place this
  parser's tokenizing and grammar concerns are not cleanly separable.
- **`\b{...}`'s empty-name backtrack.** `\b{2}` is a bare word boundary
  followed by a `{2}` repetition, not a malformed named boundary; the
  original parser detected this by rewinding `*regex-position*` to just
  after `\b` when no name followed the `{`. Splitting `\b` and a following
  `{` into two independent tokens (rather than one `\b{`-prefixed token)
  turns that rewind into simply *not consuming* the `:LBRACE` token when the
  name turns out to be empty -- no position to restore, since nothing
  committed to the brace in the first place.

Every other escape shape, POSIX class, and set operator is fully resolved in
the tokenizer, using plain raw-index scanning wherever the original used
`raw-peek` rather than `peek` (POSIX class names, `\Q...\E` bodies, octal
digit runs) -- those were never extended-mode-sensitive to begin with, so
the tokenizer's one-pass, flag-independent model matches the original
exactly. Class set operators (`&&`, `~~`, `--`) are a related but distinct
case: they are lexically unambiguous, but the original only tests for them
at true item-boundary positions (`parse-union`'s loop top), never mid-item,
so `[a-&&b]`'s second `&` (consumed as a range endpoint) is never
re-examined as the start of a fresh `&&`. `regex-grammar-classes.lisp`'s
`CLASS-SET-OPERATOR` reproduces that boundary-only check itself rather than
having the tokenizer match `&&`/`~~`/`--` wherever they happen to be
adjacent, which would not preserve it.

## Packages evaluated from the `nerima-lisp` org

- **`cl-weave`** -- adopted directly as the test-only dependency (see
  `cl-regex-kit/test` in the `.asd`); this project's fuzz and property tests
  use its `it-fuzz`/`it-property`/`gen-*` generators as-is, with no adapter
  layer.
- **`cl-nix-forge`** -- adopted directly in `flake.nix`. `mkPackageFlake`
  generates the package derivation, the `run-tests.lisp` check with its
  timeout, the treefmt-backed formatting gate, the mkdocs site and its check,
  `apps.test`/`apps.default`, and the dev shell, replacing what this file used
  to hand-write. `cl-weave` reaches the test system through
  `lispCheckDependencies` (resolved only under `doCheck`, matching
  `cl-regex-kit.asd`'s dependency-free production system), and the
  percentage-threshold coverage gate is one `extraOutputs` check built on
  `mkCommandCheck`, since threshold enforcement is domain-specific to this
  project rather than something the generic preset provides.
- **`cl-parser-kit`** -- adopted as the production parser toolkit (see [From
  a character scanner to a `cl-parser-kit` token stream](#from-a-character-scanner-to-a-cl-parser-kit-token-stream)
  above), and a real, non-test-only dependency of `cl-regex-kit` itself
  (`cl-regex-kit.asd`'s `:depends-on`, `flake.nix`'s `lispDependencies`) --
  the first this project has. Its `token`/`span` structs and
  `tokenize-regex-pattern`'s hand-rolled scanning replace the character-level
  `peek`/`take` cursor directly, with no adapter layer between them and the
  grammar; its combinator/Pratt layer is deliberately not used, for the
  reasons given above.
- **`cl-cli`** -- adopted directly for `cl-regex-kit-grep` (`cli/`), a small
  `grep`-alike over `compile-regex`/`is-match-p` that exercises the library
  as a real command-line tool. `cl-regex-kit/cli` is its own `.asd` system
  (`:depends-on ("cl-regex-kit" "cl-cli")`, `:build-operation "program-op"`)
  so the core `cl-regex-kit` system's dependency list stays exactly
  `("cl-parser-kit")` -- this is a separate delivery, not a new dependency of
  the library. `flake.nix` builds it with `cl.mkExecutable`, the same
  `packages.<name>` shape `mkPackageFlake` already produces for the library
  itself. Used directly, no adapter: `make-app`/`make-option`/
  `make-positional`/`run-app` are `cl-cli`'s own documented API.
- **`cl-boundary-kit`** -- evaluated and **not adopted**. It provides fake
  test doubles for I/O-shaped boundaries (clocks, filesystems, networks); this
  library's only "boundary" is `sb-ext:with-timeout` in `call-with-timeout`,
  which is a real-time interrupt mechanism, not a value a fake clock can
  drive. Introducing it would add a runtime dependency to a system whose
  production code depends on nothing but `cl-parser-kit`, for a boundary
  this project does not actually have.
