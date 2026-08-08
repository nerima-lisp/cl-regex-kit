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
`:nest-limit`, plus `:literal`, `:never-capture`, and `:never-newline`. For byte regexes,
`:line-terminator` also accepts an integer octet from `0` through `255`. `:literal` treats all
pattern text literally, without parsing syntax or extended-mode whitespace.
`:never-capture` makes ordinary parenthesized groups non-capturing while
retaining explicitly named captures. `:never-newline` is RE2's `never_nl` mode:
no consuming expression, including explicit newline literals, character classes,
dotall `.`, or byte `\\C`, may consume LF. `:octal` defaults to true for RE2 compatibility; set it to
`nil` to reject `\\ooo` escapes as Rust's default builder does.

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

### Reusable capture locations

```lisp
(regex-capture-locations regex) => locations
(scan-captures-into regex locations text &key (start 0) end timeout) => start, end
(scan-captures-into-at regex locations text start &key end timeout) => start, end
(capture-locations-count locations) => count
(capture-location-start locations index) => start-or-nil
(capture-location-end locations index) => end-or-nil
```

`regex-capture-locations` allocates an offset buffer for the capture count of
`regex`, including capture 0. Pass it to `scan-captures-into` to record a
leftmost-first match without retaining substrings. `scan-captures-into-at`
takes its search start as a required argument and corresponds to Rust
`Regex::captures_read_at`. The buffer is cleared before each scan: an unmatched
overall expression clears every slot, and an optional capture that does not
participate has `nil` start and end values. On success, either function returns
the overall match's start and exclusive end; on failure it returns two `nil`
values. Locations can be shared only between regexes with the same capture
count. Offsets index characters for `regex` and octets for `byte-regex`.

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

### Regex-set searches

```lisp
(regex-set-matches regex-set text &key (start 0) end timeout) => list-of-index
(regex-set-matches-at regex-set text start &key end timeout) => list-of-index
(regex-set-matches-into regex-set matches text &key (start 0) end timeout) => matches
(regex-set-match-p regex-set text &key (start 0) end timeout) => boolean
(regex-set-match-at-p regex-set text start &key end timeout) => boolean
```

`regex-set-matches` returns every source-pattern index that matches `text` at
within `[start,end)`, in source order. Duplicate source patterns therefore produce
distinct indexes. `regex-set-matches-at` makes `start` a required positional
argument and corresponds to Rust `RegexSet::matches_at`. `regex-set-match-p`
is the boolean form when only whether at least one member matches is needed;
it stops at the first matching member instead of collecting every index.
`regex-set-match-at-p` is its Rust `RegexSet::is_match_at` counterpart. A byte
regex set requires `text` to be an octet vector; a character regex set requires
a string.

`regex-set-matches-into` accepts a caller-owned bit vector with exactly
`regex-set-count` elements. It clears that vector and marks every matching
source-order index with `1`, avoiding the result-list allocation for repeated
scans.

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

## Conditions

### `cl-regex-kit-error`

Base condition for every error this library signals.

### `regex-syntax-error`

Signalled by `compile-regex` when a pattern cannot be parsed or compiled within
the configured resource limits. Readers:
`regex-syntax-error-pattern`, `regex-syntax-error-position`,
`regex-syntax-error-reason`.

### `regex-timeout`

Signalled when a matching operation exceeds its `:timeout`. The
`regex-timeout-seconds` reader returns the requested limit.
