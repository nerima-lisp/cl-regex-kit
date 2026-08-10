# API Reference

All symbols live in the `cl-regex-kit` package.

## Compilation

### `compile-regex`

```lisp
(compile-regex pattern) => regex
```

Parses and compiles `pattern` (a string) into a `regex` object ready for
`scan`. Signals `regex-syntax-error` if `pattern` cannot be parsed or exceeds
the compiler's resource limits. Builder options include `:case-insensitive`,
`:multi-line`, `:dot-matches-new-line`, `:swap-greed`, `:ignore-whitespace`,
`:unicode`, `:crlf`, `:octal`, `:line-terminator`, `:size-limit`, and
`:nest-limit`, and `:callout`, plus `:literal`, `:never-capture`, and
`:never-newline`. `:callout` supplies an optional callback for advanced
PCRE2-style callout nodes; it receives `(number tag position text)` and may
return `NIL`, `:continue`, or `:fail`. For byte regexes,
`:line-terminator` also accepts an integer octet from `0` through `255`. `:literal` treats all
pattern text literally, without parsing syntax or extended-mode whitespace.
`:never-capture` makes ordinary parenthesized groups non-capturing while
retaining explicitly named captures. `:never-newline` is RE2's `never_nl` mode:
no consuming expression, including explicit newline literals, character classes,
dotall `.`, or byte `\\C`, may consume LF. `:octal` defaults to true for RE2 compatibility; set it to
`nil` to reject `\\ooo` escapes as Rust's default builder does.

An advanced pattern can invoke the `:callout` callback at a PCRE2-style callout
node. The callback observes the callout number, optional tag, current position,
and input text, then returns `:continue` (or `NIL`) to keep the match path or
`:fail` to reject it:

```lisp
(let ((events nil))
  (let ((regex (cl-regex-kit:compile-regex
                "(?C7)a"
                :callout
                (lambda (number tag position text)
                  (push (list number tag position text) events)
                  :continue))))
    (cl-regex-kit:scan regex "a")
    (nreverse events)))
;; => ((7 NIL 0 "a"))
```

Balancing groups use .NET-compatible syntax. For example,
`(?<open>a)+(?<-open>b)+` matches `aabb`; a balancing group with no capture
to pop fails. `regex-callout` reads the configured callback back off a
compiled pattern.

### `compile-byte-regex`

```lisp
(compile-byte-regex pattern) => regex
```

Compiles `pattern` for `(array (unsigned-byte 8) (*))` input. Use `scan`,
`all-matches`, `do-matches`, `split`, and capture accessors exactly as for a
string regex; returned matched text is an octet vector. `\\C` matches any
octet, including a newline and bytes that are not valid UTF-8, unless
`:never-newline t` is enabled. Byte patterns
match valid UTF-8 scalars with Unicode-aware constructs by default. Use
`(?-u:...)` to scope raw octet matching; within that scope shorthands and case
folding are ASCII-only. Direct non-ASCII literals and Unicode escapes (`\\x{...}`,
`\\u...`, `\\U...`) in that scope encode to UTF-8; use `\\xHH` or octal for an
exact raw byte. Character classes in that scope reject non-ASCII literals and
Unicode escapes that resolve to non-ASCII scalars. `split`, `replace-first`,
`replace-all`, and `replace-n` retain the
octet-vector type. It accepts the same builder options as `compile-regex`.
Replacement templates are octet vectors with ASCII `$0`,
`$name`, `${name}`, and `$$` capture syntax.

### `byte-regex`

```lisp
(byte-regex pattern &rest compile-options) => regex
```

Load-time compiled form of `compile-byte-regex`, analogous to `regex`.

### `escape`

```lisp
(escape string) => escaped-pattern
```

Quotes Rust `regex-syntax` metacharacters (`\\.^$|?*+()[]{}#&-~`) in `string`.
The returned pattern matches the input literally in normal regex mode. To
embed arbitrary text in `(?x)` extended mode, explicitly quote its whitespace.

### `regex`

A CLOS class representing a compiled pattern. `regex-source` returns a fresh
copy of the original pattern; `regex-group-count` returns the number of explicit
capture groups. `regex-capture-count` returns the total number of capture
groups, including capture 0 for the overall match. `regex-capture-names` returns a new vector indexed by
capture number: index 0 and unnamed captures are `nil`, and named captures
contain their names. `regex-group-index` maps a named capture to its numeric
index, or returns `nil` when no such name exists. Use `(regex-p object)` to
test whether an object is a compiled pattern.

`regex-static-capture-count` returns the total number of captures that
participate in every possible match, including capture 0, or `nil` if that
number varies. It corresponds to Rust `Regex::static_captures_len`.

### `byte-regex-p`

```lisp
(cl-regex-kit:byte-regex-p regex)
  => generalized-boolean
```

Reports whether `regex` matches octet vectors rather than strings. `regex` is
a compiled pattern.

**Returns**: true when `regex` came from `compile-byte-regex` or the
`byte-regex` macro, `nil` for a character regex. Every search and replacement
entry point consults it to decide which input type it accepts.

**Signals**: `none` for a compiled pattern. Any other argument signals a
no-applicable-method error rather than `type-error`, because this is a CLOS
slot reader.

**Example**:

```lisp
(cl-regex-kit:byte-regex-p (cl-regex-kit:compile-byte-regex "a")) ; => T
(cl-regex-kit:byte-regex-p (cl-regex-kit:compile-regex "a")) ; => NIL
```

See also: [`compile-byte-regex`](api.md#compile-byte-regex), [`byte-match`](api.md#byte-match)

### `regex-advanced-p`

```lisp
(cl-regex-kit:regex-advanced-p regex)
  => generalized-boolean
```

Reports whether `regex` runs on the bounded advanced matcher instead of the
Thompson NFA and Pike VM. This is how a caller determines whether a given
compiled pattern still carries this library's central guarantee: `nil` means
the pattern compiled to an NFA program, so matching time stays linear in the
input length no matter what the input is.

A true value means the pattern uses a construct no finite automaton can
represent, and is executed by ordered backtracking bounded by
`regex-advanced-step-limit` and `regex-advanced-nest-limit` rather than by a
linear-time guarantee. The selecting constructs are backreferences,
lookaround, atomic groups, possessive repetition, conditionals, subroutine
calls, control verbs, callouts, `\\K`, `\\X`, balancing groups, and the `\\G`,
`\\Z`, `\\b{g}`, `\\b{wb}`, and `\\b{sb}` anchors. Everything else, including
plain `\\b`, `\\A`, and `\\z`, stays on the linear-time path. `compile-regex`
decides this once, from the parsed pattern, and the value never changes for a
given `regex`.

A true value also carries a caller obligation. On the advanced path the step
and state budgets are consumed across the whole scan rather than reset at each
start position, so a long enough subject exhausts them and signals
`advanced-regex-limit-error` instead of returning no-match, with no adversarial
pattern involved. Handle that condition around advanced-path calls that take
caller-supplied input. [Conditions](conditions.md#advanced-executor-limits)
explains the consequences and what to do about them.

**Returns**: a generalized boolean, not always `t`. For a pattern selected by
an anchor or a balancing group the true value is the internal marker that made
the decision, so test for non-`nil` rather than comparing against `t`.

**Signals**: `none` for a compiled pattern. Any other argument signals a
no-applicable-method error rather than `type-error`, because this is a CLOS
slot reader.

**Example**:

```lisp
(cl-regex-kit:regex-advanced-p (cl-regex-kit:compile-regex "a+b")) ; => NIL
(cl-regex-kit:regex-advanced-p (cl-regex-kit:compile-regex "(?=a)a")) ; => T
```

See also: [`regex-advanced-step-limit`](api.md#regex-advanced-step-limit), [`compile-regex`](api.md#compile-regex), [Conditions](conditions.md#advanced-executor-limits)

### `regex-advanced-step-limit`

```lisp
(cl-regex-kit:regex-advanced-step-limit regex)
  => positive-integer
```

Returns the `:size-limit` value `regex` was compiled with. On the advanced
path this one number bounds two counters: the evaluation steps taken and the
number of live states created.

**Returns**: the effective limit, defined for every compiled pattern but
consulted only when `regex-advanced-p` is true. Exceeding it during matching
signals `advanced-regex-limit-error` with kind `:steps` or `:states`. On the
NFA path the same `:size-limit` instead caps the compiled instruction count at
compile time.

**Signals**: `none` for a compiled pattern. Any other argument signals a
no-applicable-method error rather than `type-error`, because this is a CLOS
slot reader.

**Example**:

```lisp
(cl-regex-kit:regex-advanced-step-limit
 (cl-regex-kit:compile-regex "(?=a)a" :size-limit 5000))
;; => 5000
```

See also: [`regex-advanced-p`](api.md#regex-advanced-p), [Conditions](conditions.md)

### `regex-advanced-nest-limit`

```lisp
(cl-regex-kit:regex-advanced-nest-limit regex)
  => non-negative-integer
```

Returns the `:nest-limit` value `regex` was compiled with, which bounds
evaluation nesting depth on the advanced path.

**Returns**: the effective limit, defined for every compiled pattern but
consulted only when `regex-advanced-p` is true. Exceeding it during matching
signals `advanced-regex-limit-error` with kind `:nest-depth`. The same
`:nest-limit` separately caps parser nesting at compile time.

**Signals**: `none` for a compiled pattern. Any other argument signals a
no-applicable-method error rather than `type-error`, because this is a CLOS
slot reader.

**Example**:

```lisp
(cl-regex-kit:regex-advanced-nest-limit
 (cl-regex-kit:compile-regex "(?=a)a" :nest-limit 32))
;; => 32
```

See also: [`regex-advanced-p`](api.md#regex-advanced-p), [Conditions](conditions.md)

### `regex-never-newline-p`

```lisp
(cl-regex-kit:regex-never-newline-p regex)
  => generalized-boolean
```

Reports whether `regex` was compiled with `:never-newline t`. Every search
reads the value back out of the `regex` and applies it, so this reader is the
authoritative record of whether the pattern may consume LF.

**Returns**: `t` when the pattern was compiled with `:never-newline t`, `nil`
otherwise.

**Signals**: `none` for a compiled pattern. Any other argument signals a
no-applicable-method error rather than `type-error`, because this is a CLOS
slot reader.

**Example**:

```lisp
(cl-regex-kit:regex-never-newline-p
 (cl-regex-kit:compile-regex "." :never-newline t))
;; => T
```

See also: [`compile-regex`](api.md#compile-regex)

### `regex-callout`

```lisp
(cl-regex-kit:regex-callout regex)
  => function-or-nil
```

Returns the function supplied as `:callout`, which the advanced path invokes
at a PCRE2-style callout node.

**Returns**: the configured function, or `nil` when none was supplied. Only
the advanced execution path invokes it, and a configured callout the pattern
never reaches is never called.

**Signals**: `none` for a compiled pattern. Any other argument signals a
no-applicable-method error rather than `type-error`, because this is a CLOS
slot reader.

**Example**:

```lisp
(cl-regex-kit:regex-callout (cl-regex-kit:compile-regex "a")) ; => NIL
```

See also: [`compile-regex`](api.md#compile-regex)

### `regex-capture-locations`

```lisp
(cl-regex-kit:regex-capture-locations regex)
  => capture-locations
```

Allocates a reusable offset buffer sized for the capture count of `regex`,
including capture 0. A buffer records offsets rather than substrings, so it can
be reused across different input texts and avoids allocating a result per
match.

**Returns**: a fresh `capture-locations` buffer whose slots all start `nil`. A
buffer can be shared only between regexes with the same capture count.

**Signals**: `type-error` when `regex` is not a compiled pattern.

**Example**:

```lisp
(cl-regex-kit:capture-locations-count
 (cl-regex-kit:regex-capture-locations (cl-regex-kit:compile-regex "(a)(b)?")))
;; => 3
```

See also: [`scan-captures-into`](api.md#scan-captures-into), [`capture-locations-p`](api.md#capture-locations-p)

### `scan-captures-into`

```lisp
(cl-regex-kit:scan-captures-into regex locations text &key start end timeout)
  => (values start end)
```

Scans `text` for the leftmost-first match of `regex` and writes the capture
offsets into `locations`, without retaining any substring.

| Argument | Type | Default | Meaning |
|---|---|---|---|
| `regex` | `regex` | — | Compiled pattern to search with |
| `locations` | `capture-locations` | — | Buffer from `regex-capture-locations`, with the same capture count as `regex` |
| `text` | `string` or octet vector | — | Input, matching the mode of `regex` |
| `start` | `integer` | `0` | Inclusive lower bound of the search range |
| `end` | `(or null integer)` | `nil` | Exclusive upper bound; `nil` means end of `text` |
| `timeout` | `(or null (real (0) *))` | `nil` | Deadline in seconds; `nil` imposes none |

**Returns**: on a match, two values — the overall match's start and exclusive
end. On no match it returns a single `nil`. The buffer is cleared before every
scan, so an unmatched expression leaves every slot `nil`, a capture that did
not participate has `nil` start and end, and no value survives from an earlier
scan. Offsets index characters for a character regex and octets for a byte
regex.

**Signals**: `type-error` (`regex` is not a pattern, `locations` has a
different capture count than `regex`, `text` is the wrong type, or `start`,
`end`, or `timeout` is out of range), `regex-timeout` (`timeout` elapses).

**Example**:

```lisp
(let* ((regex (cl-regex-kit:compile-regex "(a)(b)?"))
       (locations (cl-regex-kit:regex-capture-locations regex)))
  (multiple-value-list (cl-regex-kit:scan-captures-into regex locations "ab")))
;; => (0 2)
```

See also: [`regex-capture-locations`](api.md#regex-capture-locations), [`scan`](api.md#scan)

### `scan-captures-into-at`

```lisp
(cl-regex-kit:scan-captures-into-at regex locations text start &key end timeout)
  => (values start end)
```

The same operation as `scan-captures-into` with the search start as a required
positional argument. This is the Rust `Regex::captures_read_at` equivalent.

**Returns**: identical to `scan-captures-into` — the overall start and
exclusive end on a match, a single `nil` otherwise.

**Signals**: the same conditions as `scan-captures-into`.

**Example**:

```lisp
(let* ((regex (cl-regex-kit:compile-regex "(a)(b)?"))
       (locations (cl-regex-kit:regex-capture-locations regex)))
  (multiple-value-list
   (cl-regex-kit:scan-captures-into-at regex locations "zab" 1)))
;; => (1 3)
```

See also: [`scan-captures-into`](api.md#scan-captures-into)

### `capture-locations-count`

```lisp
(cl-regex-kit:capture-locations-count locations)
  => positive-integer
```

Returns the number of capture slots in `locations`, counting group 0.

**Returns**: the slot count, which equals the `regex-capture-count` of the
pattern the buffer was allocated for.

**Signals**: `type-error` when `locations` is not a `capture-locations` buffer.

See also: [`regex-capture-locations`](api.md#regex-capture-locations)

### `capture-location-start`

```lisp
(cl-regex-kit:capture-location-start locations index)
  => integer-or-nil
```

Returns the recorded start offset for capture `index`, where `index` is a
non-negative integer below `capture-locations-count`.

**Returns**: the start offset, or `nil` when that capture did not participate
in the last scan or no scan has succeeded.

**Signals**: `type-error` when `locations` is not a buffer, or when `index` is
negative or not below the slot count.

See also: [`capture-location-end`](api.md#capture-location-end), [`scan-captures-into`](api.md#scan-captures-into)

### `capture-location-end`

```lisp
(cl-regex-kit:capture-location-end locations index)
  => integer-or-nil
```

Returns the recorded exclusive end offset for capture `index`.

**Returns**: the exclusive end offset, or `nil` under the same conditions that
make `capture-location-start` return `nil`.

**Signals**: `type-error` when `locations` is not a buffer, or when `index` is
negative or not below the slot count.

See also: [`capture-location-start`](api.md#capture-location-start)

### `regex` macro

```lisp
(regex literal-pattern &rest literal-compile-options) => regex
```

Compiles a literal pattern and compile-time constant keyword options once when
its containing file is loaded. Dynamic patterns or option expressions must use
`compile-regex`. `regex` is also the name of the CLOS class; Common Lisp keeps
macro and type namespaces separate.

### `compile-regex-set`, `compile-byte-regex-set`, and set macros

```lisp
(compile-regex-set patterns &rest compile-options) => regex-set
(regex-set literal-pattern* &rest literal-compile-options) => regex-set
(compile-byte-regex-set patterns &rest compile-options) => regex-set
(byte-regex-set literal-pattern* &rest literal-compile-options) => regex-set
```

Compiles a list or a non-string vector of patterns as an immutable multi-pattern
set. An empty set is valid and never reports a match.
`compile-options` are the same keyword arguments accepted by `compile-regex`,
applied to every pattern. `:size-limit` also bounds the final merged NFA for
the whole set. The `regex-set` macro requires string literals and compiles
them and compile-time constant keyword options once when its containing file
is loaded.

`regex-set-p` tests the type. `regex-set-patterns` returns a fresh vector of
fresh source-pattern strings, so callers cannot mutate a compiled set.
`regex-set-count` and `regex-set-empty-p` inspect its cardinality without
copying those patterns. The byte forms use `compile-byte-regex` syntax and
options, match octet vectors, and accept valid UTF-8 scalars for Unicode-aware
constructs by default. `(?-u:...)` and `\\C` match raw bytes including invalid
UTF-8. `byte-regex-set-p` identifies those sets.

## Matching

All searching and match-processing operations accept `:start` and `:end`.
They define a half-open search range, `[start, end)`: a returned match must
start and end inside that range. Anchors and word boundaries continue to inspect
the original input, so `:end` does not turn a range boundary into end-of-text.

### `scan`

```lisp
(scan regex text &key (start 0) end timeout) => match-result-or-nil
```

Finds the leftmost-first match of `regex` in `text` at or after `start`.
`timeout`, when supplied, is a positive number of seconds. Expiry signals
`regex-timeout`; the default `nil` imposes no deadline.

### `shortest-match`

```lisp
(shortest-match regex text &key (start 0) end timeout) ; => end-index-or-nil
```

Returns the earliest ending match at the leftmost position at or after
`start`. This is equivalent to Rust `Regex::shortest_match`: for `a+` in
`"aaa"`, it returns `1`, whereas `scan` selects the normal greedy match.
It accepts both text and byte regexes and returns `nil` if no match exists.

### `shortest-match-at`

```lisp
(shortest-match-at regex text start &key end timeout) ; => end-index-or-nil
```

Return the earliest ending match at the leftmost position at or after the
required `start` position. This corresponds to Rust `Regex::shortest_match_at`;
`end`, when supplied, is the exclusive upper bound of the searched range.

### `longest-match`

```lisp
(longest-match regex text &key (start 0) end timeout) ; => match-result-or-nil
```

Returns the longest match at the leftmost position at or after `start`. This
is the selection policy exposed by RE2's `longest_match` option. `scan` keeps
Rust-style leftmost-first selection, so use `longest-match` only when POSIX or
RE2 longest-match behavior is required. Equal-length alternatives retain the
normal greedy/lazy branch and capture priority. It accepts both text and byte
regexes and returns `nil` if no match exists.

### `is-match-p`

```lisp
(is-match-p regex text &key (start 0) end timeout) => boolean
```

Boolean form of `scan`, without constructing an application-level branch on a
`match-result`.

### `is-match-at`

```lisp
(is-match-at regex text start &key end timeout) => boolean
```

Return whether `regex` matches at or after the required `start` position.
This corresponds to Rust `Regex::is_match_at`; `end`, when supplied, is the
exclusive upper bound of the searched range.

### `captures` and `captures-at`

```lisp
(captures regex text &key (start 0) end timeout) => match-result-or-nil
(captures-at regex text start &key end timeout) => match-result-or-nil
```

Rust-compatible names for a capture-aware single search. Both return the same
`match-result` as `scan`, including all participating and nonparticipating
capture groups; use `match-captures` or `match-group-*` to inspect them.
`captures-at` takes its search start as a required argument, corresponding to
Rust `Regex::captures_at`.

### `scan` and `scan-at`

```lisp
(scan regex text &key (start 0) end timeout) => match-result-or-nil
(scan-at regex text start &key end timeout) => match-result-or-nil
```

`scan-at` takes its search start as a required argument and corresponds to
Rust `Regex::find_at`. It has the same leftmost-first result and range
semantics as `scan`.

### `full-match` and `full-match-p`

```lisp
(full-match regex text &key (start 0) end timeout) => match-result-or-nil
(full-match-p regex text &key (start 0) end timeout) => boolean
```

`full-match` returns a `match-result` only when some matching path spans the
entire selected `[start,end)` range. It uses leftmost-longest selection at the
range start, so a complete alternative remains visible even when an earlier
alternative is shorter. `full-match-p` is its boolean predicate. Omitting
`end` selects the rest of `text`. Anchors still use the original input's
boundaries, rather than treating the selected range as a new string.

### `match`

```lisp
(match pattern text &key (start 0) end timeout) => match-result-or-nil
```

Convenience wrapper: `(scan (compile-regex pattern) text :start start :end end)`. Prefer
`compile-regex` + `scan` when matching the same pattern repeatedly.

### `byte-match`

```lisp
(cl-regex-kit:byte-match pattern text &key start end timeout)
  => match-result-or-nil
```

The octet-vector counterpart of `match`: compiles `pattern` with
`compile-byte-regex` and scans it once over `text`. Offsets and `match-string`
are in octets, and the same one-shot compilation cost applies, so prefer
`compile-byte-regex` plus `scan` when matching the same pattern repeatedly.

| Argument | Type | Default | Meaning |
|---|---|---|---|
| `pattern` | `string` | — | Byte-regex source, compiled by `compile-byte-regex` |
| `text` | `(array (unsigned-byte 8) (*))` | — | Octet vector to search |
| `start` | `integer` | `0` | Inclusive lower bound of the search range |
| `end` | `(or null integer)` | `nil` | Exclusive upper bound; `nil` means end of `text` |
| `timeout` | `(or null (real (0) *))` | `nil` | Deadline in seconds; `nil` imposes none |

**Returns**: a `match-result` whose offsets are octet indexes, or `nil` when
`pattern` does not match within `[start, end)`.

**Signals**: `regex-syntax-error` (`pattern` cannot be parsed or exceeds a
compile limit), `type-error` (`text` is not an octet vector, or `start`, `end`,
or `timeout` is out of range), `regex-timeout` (`timeout` elapses).

**Example**:

```lisp
(let ((text (make-array 4
                        :element-type '(unsigned-byte 8)
                        :initial-contents '(#xff #x41 #x42 #x80))))
  (let ((result (cl-regex-kit:byte-match "(?-u:\\x41\\x42)" text :start 1 :end 3)))
    (list (cl-regex-kit:match-start result)
          (cl-regex-kit:match-end result)
          (coerce (cl-regex-kit:match-string result text) 'list))))
;; => (1 3 (65 66))
```

See also: [`match`](api.md#match), [`compile-byte-regex`](api.md#compile-byte-regex), [`byte-regex-p`](api.md#byte-regex-p)

### `capture-locations-p`

```lisp
(cl-regex-kit:capture-locations-p object)
  => boolean
```

Tests whether `object` is a reusable capture-offset buffer. `object` may be
any value.

**Returns**: `t` for a buffer produced by `regex-capture-locations` or bound by
`do-captures`, `nil` for everything else. The structure has no exported
constructor, so a true result also establishes where the buffer came from.

**Signals**: `none`. The predicate accepts any object, including `nil`.

**Example**:

```lisp
(cl-regex-kit:capture-locations-p
 (cl-regex-kit:regex-capture-locations (cl-regex-kit:compile-regex "(a)")))
;; => T
```

See also: [`regex-capture-locations`](api.md#regex-capture-locations)

### `all-matches`, `do-matches`, and `do-captures`

```lisp
(all-matches regex text &key (start 0) end timeout) => list-of-match-result
(do-matches (result regex text &key (start 0) end timeout) form*) => nil
(do-captures (locations regex text &key (start 0) end timeout) form*) => nil
```

Both traverse non-overlapping matches left to right. `all-matches` collects
results; `do-matches` processes the stream incrementally without constructing a
result list. `do-captures` is the equivalent of Rust's `captures_iter`: it
binds `locations` once to a reusable `capture-locations` buffer and updates it
for each match. A group that does not participate in the current match has
`nil` start and end offsets; values never carry over from an earlier iteration.
Capture offsets remain valid only until the next iteration.
As with Rust's regex iterators, an empty match immediately after a
non-empty match is omitted and the next search starts one position later. Thus
`a*` over `"aba"` produces spans `(0 1)` and `(2 3)`. Empty-only expressions
still match once at every input boundary. The timeout covers the complete
iteration, including all matches.

### `regex-set-matches`

```lisp
(cl-regex-kit:regex-set-matches regex-set text &key start end timeout)
  => list-of-index
```

Returns every source-pattern index in `regex-set` that matches `text` within
`[start, end)`.

| Argument | Type | Default | Meaning |
|---|---|---|---|
| `regex-set` | `regex-set` | — | Compiled multi-pattern set |
| `text` | `string` or octet vector | — | Input, matching the mode of `regex-set` |
| `start` | `integer` | `0` | Inclusive lower bound of the search range |
| `end` | `(or null integer)` | `nil` | Exclusive upper bound; `nil` means end of `text` |
| `timeout` | `(or null (real (0) *))` | `nil` | Deadline in seconds; `nil` imposes none |

**Returns**: a fresh list of indexes in source order, or `nil` when no member
matches. Duplicate source patterns occupy distinct indexes and are reported
separately. A byte regex set requires an octet vector; a character regex set
requires a string.

**Signals**: `type-error` (`regex-set` is not a set, `text` is the wrong type,
or `start`, `end`, or `timeout` is out of range), `regex-timeout` (`timeout`
elapses).

**Example**:

```lisp
(cl-regex-kit:regex-set-matches
 (cl-regex-kit:compile-regex-set '("a" "b" "a")) "ab")
;; => (0 1 2)
```

See also: [`regex-set-match-p`](api.md#regex-set-match-p), [`regex-set-matches-into`](api.md#regex-set-matches-into)

### `regex-set-matches-at`

```lisp
(cl-regex-kit:regex-set-matches-at regex-set text start &key end timeout)
  => list-of-index
```

The same search as `regex-set-matches` with the search start as a required
positional argument. This is the Rust `RegexSet::matches_at` equivalent.

**Returns**: a fresh list of matching source-order indexes, or `nil` when no
member matches.

**Signals**: the same conditions as `regex-set-matches`.

**Example**:

```lisp
(cl-regex-kit:regex-set-matches-at
 (cl-regex-kit:compile-regex-set '("a" "b" "a")) "zab" 1)
;; => (0 1 2)
```

See also: [`regex-set-matches`](api.md#regex-set-matches)

### `regex-set-matches-into`

```lisp
(cl-regex-kit:regex-set-matches-into regex-set matches text &key start end timeout)
  => matches
```

Records the matching source-order indexes into a caller-owned bit vector,
avoiding the result-list allocation when the same set is scanned repeatedly.

| Argument | Type | Default | Meaning |
|---|---|---|---|
| `regex-set` | `regex-set` | — | Compiled multi-pattern set |
| `matches` | `bit-vector` | — | Caller-owned vector with exactly `regex-set-count` elements |
| `text` | `string` or octet vector | — | Input, matching the mode of `regex-set` |
| `start` | `integer` | `0` | Inclusive lower bound of the search range |
| `end` | `(or null integer)` | `nil` | Exclusive upper bound; `nil` means end of `text` |
| `timeout` | `(or null (real (0) *))` | `nil` | Deadline in seconds; `nil` imposes none |

**Returns**: the same `matches` vector that was passed in, not a copy. It is
cleared first, then every matching source-order index is set to `1`.

**Signals**: `type-error` (`matches` is not a bit vector or its length differs
from `regex-set-count`, `regex-set` is not a set, `text` is the wrong type, or
a range argument is invalid), `regex-timeout` (`timeout` elapses).

**Example**:

```lisp
(let ((set (cl-regex-kit:compile-byte-regex-set '("A" "\\C" "A")))
      (matches (make-array 3 :element-type 'bit :initial-element 0))
      (text (make-array 1
                        :element-type '(unsigned-byte 8)
                        :initial-contents '(65))))
  (coerce (cl-regex-kit:regex-set-matches-into set matches text) 'list))
;; => (1 1 1)
```

See also: [`regex-set-matches`](api.md#regex-set-matches)

### `regex-set-match-p`

```lisp
(cl-regex-kit:regex-set-match-p regex-set text &key start end timeout)
  => boolean
```

Reports whether at least one member of `regex-set` matches `text` within
`[start, end)`. It stops at the first matching member instead of collecting
every index.

**Returns**: `t` when any member matches, `nil` otherwise.

**Signals**: the same conditions as `regex-set-matches`.

**Example**:

```lisp
(cl-regex-kit:regex-set-match-p
 (cl-regex-kit:compile-regex-set '("a" "b")) "zz")
;; => NIL
```

See also: [`regex-set-matches`](api.md#regex-set-matches), [`regex-set-match-at-p`](api.md#regex-set-match-at-p)

### `regex-set-match-at-p`

```lisp
(cl-regex-kit:regex-set-match-at-p regex-set text start &key end timeout)
  => boolean
```

The boolean form of `regex-set-matches-at`, with the search start as a
required positional argument. This is the Rust `RegexSet::is_match_at`
equivalent.

**Returns**: `t` when any member matches at or after `start`, `nil` otherwise.

**Signals**: the same conditions as `regex-set-matches`.

**Example**:

```lisp
(cl-regex-kit:regex-set-match-at-p
 (cl-regex-kit:compile-regex-set '("a" "b")) "zab" 1)
;; => T
```

See also: [`regex-set-match-p`](api.md#regex-set-match-p)

## Text transformation

### `split`

```lisp
(split regex text &key (start 0) end timeout) => list-of-string
(split-terminator regex text &key (start 0) end timeout) => list-of-string
(split-inclusive regex text &key (start 0) end timeout) => list-of-string
(split-n regex text count &key (start 0) end timeout) => list-of-string
```

These operations split `text` at non-overlapping matches. `split` has no field limit;
`split-terminator` omits an empty final field when a delimiter matched at the
end of the input. `split-inclusive` retains each delimiter at the end of its
preceding field. `split-n` returns at most `count` fields, and a count of zero
returns no fields.
Empty matches advance by one string character or, for byte regexes, one octet, so
iteration always terminates. Consequently, an empty byte regex splits at every
octet, including between UTF-8 code units.
`start` and `end` limit where delimiter search occurs; the field before the
first delimiter still includes the prefix before `start`, and the final field
includes the suffix after `end`.

### `replace-first`, `replace-all`, and `replace-n`

```lisp
(replace-first regex text replacement
               &key (start 0) end timeout) => string
(replace-all regex text replacement
             &key (start 0) end timeout) => string
(replace-n regex text replacement count
           &key (start 0) end timeout) => string
```

`replacement` can be a function of `(match-result text)` returning a string,
or a template string. `replace-n` performs at most `count` non-overlapping
replacements, matching Rust `Regex::replacen`; `count` must be a non-negative
integer. For byte regexes, every replacement function returns an octet vector
instead of a string.

Templates use one Rust-style syntax. `$$` is a literal dollar, `$0` and `$1`
are numeric captures, and `$name` or `${name}` are named captures. A capture
that is absent or unknown expands to the empty string. Unbraced names after
`$` use only ASCII letters, digits, and `_`; use `${name}` for capture names
containing Unicode characters, `.`, `[`, or `]`. A backslash has no special
meaning, so `\1` is emitted verbatim.

Byte regexes accept octet-vector templates with the same ASCII forms.

## Reading a match

### `match-result`

A struct: `start`, `end`, `groups` (a simple-vector of `(start . end)` conses,
or `nil` for a group that did not participate).

### `match-result-p`

```lisp
(cl-regex-kit:match-result-p object)
  => boolean
```

The generated predicate of the `match-result` struct. `object` may be any
value.

**Returns**: `t` for a value returned by `scan`, `match`, `byte-match`,
`longest-match`, `full-match`, or any other capture-aware search, and `nil`
for every other object, including the `nil` those operations return when
nothing matched. Use it to check a value received from elsewhere before
passing it to `match-start`, `match-string`, or `match-captures`, each of
which signals a `type-error` on anything else.

**Signals**: `none`. The predicate accepts any object, including `nil`.

**Example**:

```lisp
(cl-regex-kit:match-result-p
 (cl-regex-kit:scan (cl-regex-kit:compile-regex "a") "a"))
;; => T
```

See also: [`match-result`](api.md#match-result), [`scan`](api.md#scan)

### `match-start`, `match-end`

The offsets bounding the whole match.

### `match-string`

```lisp
(match-string match-result text) => string
```

The substring of `text` the whole match covers.

### `match-captures`

```lisp
(match-captures match-result text) => simple-vector
```

Returns a fresh vector of capture strings in numeric order. Index zero is the
whole match; an optional capture that did not participate is `nil`. This is the
direct equivalent of iterating a Rust `Captures` value, without exposing the
engine's internal capture slots.

### `match-group-start`, `match-group-end`, `match-group-string`

```lisp
(match-group-start match-result index) => integer-or-nil
(match-group-end match-result index) => integer-or-nil
(match-group-string match-result index text) => string-or-nil
```

The offsets and substring captured by a numeric group index (1-based; group 0
is the whole match) or a capture-name string, or `nil` if that group did not
participate in the match.

### `match-mark`

```lisp
(cl-regex-kit:match-mark match-result)
  => string-or-nil
```

Returns the tag of the last `(*MARK:name)` control verb the successful match
path reached. `match-result` is a match returned by any capture-aware search.

**Returns**: the tag string, or `nil` when the path reached no mark. The short
form `(*:name)` sets the same tag. Because control verbs put a pattern on the
advanced execution path, this is always `nil` for a pattern whose
`regex-advanced-p` is `nil`.

**Signals**: `type-error` when `match-result` is anything other than a
`match-result`, including `nil`.

**Example**:

```lisp
(cl-regex-kit:match-mark
 (cl-regex-kit:scan (cl-regex-kit:compile-regex "a(*MARK:middle)b") "ab"))
;; => "middle"
```

See also: [`regex-advanced-p`](api.md#regex-advanced-p), [`match-result`](api.md#match-result)

## Conditions

Every condition this library signals inherits from `cl-regex-kit-error`, so a
caller can handle that single class to catch any failure from the library. The
exported condition types are `regex-syntax-error` for a pattern that cannot be
parsed or compiled, `regex-timeout` for a matching operation that exceeds its
`:timeout`, and `advanced-regex-limit-error` for an advanced pattern that
exhausts a step, state, or nesting budget.

[Conditions](conditions.md) documents each type and its readers, with handling
examples. The **Signals** field of each entry above names the conditions that
entry can raise.
