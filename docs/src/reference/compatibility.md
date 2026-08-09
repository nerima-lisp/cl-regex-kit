# Compatibility

`cl-regex-kit` has two execution paths. Thompson NFA construction and
Pike's VM simulation, the same foundation as RE2 and Rust's `regex` crate,
handle the regular subset with a linear-time guarantee. Patterns that require
capture-dependent or ordered-backtracking semantics are routed to a bounded
advanced executor. The advanced path expands compatibility without pretending
that every pattern still has the NFA path's worst-case bound.

## Implemented RE2/Rust-style syntax

- Literals, alternation, capturing and non-capturing groups
- Named captures: `(?<name>...)` and `(?P<name>...)`; names use Rust's
  grammar: an underscore or Unicode alphabetic code point followed by Unicode
  alphabetic code points, Unicode numbers (`Nd`, `Nl`, or `No`), plus `.`,
  `_`, `[`, and `]`; they work with `match-group-*` and
  `regex-group-index`. Unbraced `$name` replacement templates accept only
  ASCII letters, digits, and `_`; use `${name}` for every other valid capture
  name.
- Greedy and lazy `*`, `+`, `?`, `{m}`, `{m,n}`, and `{m,}` repetition.
  Finite bounds are limited to 1000 for RE2 compatibility; `:size-limit`
  separately bounds the compiled NFA, following Rust regex's resource model.
- Character classes, ranges, ASCII POSIX classes (`alnum`, `alpha`, `ascii`,
  `blank`, `cntrl`, `digit`, `graph`, `lower`, `print`, `punct`, `space`,
  `upper`, `word`, and `xdigit`, including inner negation), plus intersection (`&&`), difference (`--`),
  and symmetric-difference (`~~`) set operations with nested or direct right
  operands; empty classes (`[]`) denote the empty set, and negated empty
  classes (`[^]`) denote the full alphabet.
- Unicode-aware `\\d`/`\\w`/`\\s` shorthands, `\\pX`/`\\PX` and `\\p{...}`/`\\P{...}` Unicode
  general-category, Script, Block, Age, Grapheme_Cluster_Break, Word_Break,
  Sentence_Break, selected binary properties, and Rust-compatible property-value
  separators: `=`, `:`, and `!=`; `!=` composes with the outer `\\p` or `\\P`
  negation; `\\b`/`\\B` boundaries, Rust-style `\\b{start}`/`\\b{end}`/`\\b{start-half}`/`\\b{end-half}` boundary variants,
  and RE2-style `\\<`/`\\>` word boundaries
- Extended character escapes: `\\h`/`\\H` for Unicode horizontal whitespace,
  `\\N` for a non-newline character, `\\R` for a line-break sequence (including
  CRLF as one consuming unit), and named characters such as
  `\\N{LATIN CAPITAL LETTER A}`
- `\\a`, `\\f`, `\\n`, `\\r`, `\\t`, `\\v`, one- through three-digit `\\ooo`, braced octal `\\o{...}`, `\\xHH`, `\\x{...}`, `\\uHHHH`,
  `\\u{...}`, `\\UHHHHHHHH`, and `\\U{...}` escapes (including character classes), plus RE2-style
  `\\Q...\\E` quoted literals
- `.` and inline `i`, `m`, `s`, `R`, `U`, `x`, and `u` flags, including scoped and disabling
  forms such as `(?im-s:...)`
- In `(?x)` mode, Unicode whitespace and `#` line comments are ignored throughout a
  pattern, including character classes; use `\\ ` or `\\x20` for a literal space and
  `\\#` for a literal number sign
- `^`/`$` and absolute `\\A`/`\\z` anchors
- `escape` for safely embedding an arbitrary literal string in a pattern
- Capture metadata through `regex-capture-count`, `regex-capture-names`, and
  `regex-static-capture-count` (the Rust `Regex::static_captures_len`
  equivalent), plus reusable offset buffers from `regex-capture-locations`
  and `scan-captures-into` / `scan-captures-into-at`
- Leftmost-first scanning through `scan` / `scan-at`, Rust-compatible capture
  searches through `captures` / `captures-at`, position-aware boolean matching
  through `is-match-at`, Rust-style
  shortest matching through `shortest-match` / `shortest-match-at`, RE2-style
  leftmost-longest matching through `longest-match`, RE2-style complete matching
  through `full-match` /
  `full-match-p`, non-overlapping iteration, `split`, `split-terminator`,
  `split-inclusive`, and `split-n`, and
  first, bounded, and all replacement through `replace-first`, `replace-n`,
  and `replace-all`, with Rust-style capture templates (`$0`, `$name`,
  `${name}`, and `$$`)
- RE2/Rust-style multi-pattern matching through `compile-regex-set`,
  `regex-set-count`, `regex-set-empty-p`, `regex-set-matches`,
  `regex-set-matches-at`, `regex-set-matches-into`, `regex-set-match-p`, and
  `regex-set-match-at-p`; duplicate patterns retain their individual source
  indexes

## Advanced ordered-backtracking syntax

The advanced executor handles constructs that cannot be represented by the
regular NFA alone:

- Numeric and named backreferences, including `\g{n}`, `\k<name>`,
  and quoted subroutine names with `\g'name'`
- Positive and negative lookahead and lookbehind, including fixed- and
  variable-length lookbehind
- Extended grapheme clusters (`\X`), possessive quantifiers, and atomic
  groups
- Subroutine calls and recursion: `(?R)`, `(?&name)`,
  `(?P>name)`, relative calls, and recursion conditions
- DEFINE blocks, capture conditions, and branch-reset groups
  (`(?|...)`)
- Match-position and end-of-input controls: `\K`, `\G`, and
  `\Z`
- PCRE-style control verbs: `(*FAIL)`, `(*SKIP)`,
  `(*PRUNE)`, `(*COMMIT)`, `(*THEN)`,
  `(*ACCEPT)`, and `(*MARK:tag)`, including their short
  aliases where defined by the parser

The advanced executor honors `:size-limit` as a maximum evaluation-step
budget and `:nest-limit` as the recursion-depth limit. A timeout can
also be supplied through the existing matching APIs. These limits are
resource safeguards, not a claim of linear-time matching for arbitrary
backtracking patterns.

## Compilation options

`compile-regex` and `compile-regex-set` accept builder-style keyword options for the initial parsing
mode: `:case-insensitive`, `:multi-line`, `:dot-matches-new-line`,
`:swap-greed`, `:ignore-whitespace`, `:unicode`, `:crlf`, `:octal`, and
`:line-terminator`, plus RE2-compatible `:literal`, `:never-capture`, and
`:never-newline`.
`:literal t` treats the entire source pattern as literal text, including
whitespace and metacharacters. `:never-capture t` treats ordinary `(...)`
groups as non-capturing but preserves explicitly named captures.
`:never-newline t` is RE2's `never_nl` behavior. It prevents every consuming
expression, including explicit newline literals, character classes, dotall `.`,
and raw byte `\\C`, from consuming LF. `:line-terminator` accepts an ASCII Common Lisp character and
controls both `.` and multiline `^`/`$`; it defaults to `#\\Newline`. For byte
regexes and byte regex sets, it also accepts an integer octet from `0` through
`255`, matching Rust's byte-oriented builder. With
`:crlf t`, CRLF handling takes precedence over the configured line
terminator: both CR and LF are terminators, and anchors cannot match between
CR and LF. Inline flags remain lexical and override these initial settings
inside their group. For
example, `:case-insensitive t` makes the whole expression case-insensitive,
while `(?-i:...)` restores case-sensitive matching for the nested expression.

`:size-limit` bounds the emitted NFA instruction count and defaults to
100000. For `compile-regex-set`, it also bounds the final merged NFA,
including its set-dispatch instructions. `:nest-limit` bounds nested group
depth and defaults to 250.
Exceeding either limit signals `regex-syntax-error` before the engine attempts
matching. `:size-limit` is a program-size limit, not Rust `regex`'s
byte-oriented `RegexBuilder::size_limit`.

`:octal` defaults to `t` for RE2/Rust-compatible one- through three-digit
`\\ooo` escapes. Values outside the byte range are rejected. Set it to `nil`
to reject those escapes, matching Rust `RegexBuilder`'s default behavior.

Every matching entry point accepts `:timeout`, a positive number of seconds.
On expiry it signals `regex-timeout`; the default `nil` imposes no deadline.
This uses SBCL's timeout facility and shares the project's SBCL-only
portability boundary.

## Outside current dialect

The advanced path deliberately stops short of embedding another language or
running user code. PCRE callouts, Perl code interpolation, balancing groups,
fuzzy matching, and control verbs not listed above are outside the current
dialect. They should be rejected as syntax rather than silently compiled with
different meaning.

## Why this trade

The regular NFA path preserves the RE2/Rust guarantee for applications that
need predictable behavior on untrusted patterns or input. The advanced path is
available when compatibility with capture-dependent and ordered-backtracking
syntax is more important than that guarantee. Use `:size-limit`,
`:nest-limit`, and `:timeout` when advanced patterns are
supplied by untrusted sources.

## Replacement templates

`replace-first`, `replace-n`, and `replace-all` use one Rust-style template
syntax: `$0`, `$1`, `$name`, `${name}`, and `$$`. Backslashes are literal
characters, so `\1` is not a capture reference.

## Current differences

- Unicode property support is backed by SBCL's Unicode data. Case-insensitive
  matching uses Rust regex-syntax's generated Unicode 16 simple-case-folding
  table. General categories, Script, Block, Age, and the binary
  properties exposed by SBCL (including `Hex_Digit`, `Cased`,
  `Case_Ignorable`, `Default_Ignorable_Code_Point`, `Ideographic`, `Math`,
  `Soft_Dotted`, and `Bidi_Mirrored`) are supported. The engine also
  provides Unicode 16 range definitions for `Bidi_Control`, `Deprecated`,
  `Emoji`, `Emoji_Component`, `Emoji_Modifier`, `Emoji_Modifier_Base`,
  `Emoji_Presentation`, `Extended_Pictographic`, `Grapheme_Link`,
  `Logical_Order_Exception`, `Other_Grapheme_Extend`,
  `Prepended_Concatenation_Mark`, `Radical`, `Dash`, `Hyphen`,
  `Pattern_Syntax`, `Quotation_Mark`, `Sentence_Terminal`,
  `Terminal_Punctuation`, `Unified_Ideograph`,
  `IDS_*_Operator`, `Noncharacter_Code_Point`, `Pattern_White_Space`,
  `Regional_Indicator`, and `Variation_Selector`, including their UCD short
  aliases accepted by Rust regex (`Dia`, `IDSB`, `JoinC`, `MCM`, `OAlpha`,
  `OIDC`, `PatSyn`, `XIDS`, and `XIDC`, for example). It additionally provides
  Unicode 16 static ranges for `ID_Compat_Math_*`,
  `Indic_Conjunct_Break`, `Modifier_Combining_Mark`, and the `Other_*`
  binary properties. Segmentation properties accept UCD short and long property and value
forms, such as `GCB=RI`, `Word_Break=Katakana`, and `SB=AT`.
  `ID_Start` and `ID_Continue` are calculated from SBCL general
  categories plus the UCD-defined exceptions. `scx` and `Script_Extensions`
  use static Unicode 16 range data in
  addition to SBCL's Script property, and are therefore distinct from `sc` and
  `Script`. Script aliases such as `\\p{Grek}`, `\\p{Greek}`,
  `scx=Hira`, `Script=Greek`,
`Block=Greek_And_Coptic`, `Age=V15_0`, `Age=15.1`, `Age=V15_1`, and the Rust
alias `Age=v151` are supported. Age uses the same static Unicode 16 range data as
  Rust `regex-syntax`. Unknown names and values signal `regex-syntax-error`;
  they are never silently compiled as a class that cannot match.
- The project currently targets SBCL; portability across Common Lisp
  implementations has not been established.
- `compile-byte-regex`, `byte-regex`, `compile-byte-regex-set`, and
  `byte-regex-set` match octet vectors directly.
  Unicode-aware constructs consume valid UTF-8 scalars by default. Scoped
  `(?-u:...)` expressions and RE2's `\\C` escape consume raw octets, including
  invalid UTF-8; ASCII shorthands, word boundaries, and case folding apply
  inside non-Unicode scopes. This supports Rust's mixed Unicode/non-Unicode
  `bytes::Regex` matching model. Direct non-ASCII literals and Unicode escapes
  (`\\x{...}`, `\\u...`, `\\U...`) in a non-Unicode scope encode to UTF-8, while
  `\\xHH`, octal, and braced-octal escapes retain exact-octet semantics; non-ASCII
  character-class literals and Unicode escapes that resolve to non-ASCII
  scalars are rejected there.
  `replace-first`, `replace-n`, and `replace-all` accept
  octet-vector templates and replacement functions for byte regexes; byte
  templates support the same ASCII `$0`, `$name`, `${name}`, and `$$` capture
  forms.
  Character `Regex` values preserve Rust's UTF-8 invariant: raw scopes that
  could match invalid UTF-8 bytes are rejected at compilation. Use
  `compile-byte-regex` when raw octet matching is required.
- To retain predictable compilation resources, a compiled NFA program is
  limited to 100000 instructions. Patterns exceeding this limit signal
  `regex-syntax-error` rather than allocating without bound.
- `regex-set` merges member NFAs and scans the input once. Its Pike VM uses
  work bounded by the product of text length and total member-program size;
  this is not a DFA-based RE2 `Set` implementation.

See the [Roadmap](../project/roadmap.md) for planned extensions.
