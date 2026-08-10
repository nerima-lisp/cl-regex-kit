# API Reference

All public symbols live in the `cl-regex-kit` package. Compile a pattern
once when you reuse it; the search and transformation functions accept either
character strings or octet vectors according to the compiled pattern.

## Compilation

### `compile-regex`

**Signature:** `(compile-regex pattern &key case-insensitive multi-line
dot-matches-new-line swap-greed ignore-whitespace (unicode t) crlf literal
never-capture never-newline (octal t) (line-terminator #\Newline)
(size-limit +maximum-instruction-count+) (nest-limit +default-nest-limit+)
callout)`

Compiles a string pattern into a character `regex` object. The keyword
options control syntax compatibility, case folding, newline handling, resource
limits, and optional callouts. The pattern is parsed at runtime, so dynamic
patterns belong here rather than in the `regex` macro.

### `compile-byte-regex`

**Signature:** `(compile-byte-regex pattern &key case-insensitive multi-line
dot-matches-new-line swap-greed ignore-whitespace (unicode t) crlf literal
never-capture never-newline (octal t) (line-terminator #\Newline)
(size-limit +maximum-instruction-count+) (nest-limit +default-nest-limit+)
callout)`

Compiles a pattern for octet-vector input and returns a byte `regex` object.
It accepts the same builder options as `compile-regex`. Unicode-aware syntax
matches valid UTF-8 scalars by default; use the pattern's byte-mode constructs
when matching raw octets.

### `escape`

**Signature:** `(escape string)`

Returns a pattern that matches `string` literally by quoting regex
metacharacters. It is useful when interpolating user text into a larger
pattern. In extended mode, quote whitespace explicitly when it must remain
significant.

### `regex`

`regex` names both the compiled-pattern CLOS class and a macro. As a type,
it identifies character-pattern objects. As a macro, it compiles a literal
pattern and constant keyword options once when the containing file is loaded.
Use `compile-regex` for dynamic patterns or option values.

**Macro signature:** `(regex literal-pattern &rest literal-compile-options)`

### `byte-regex`

`byte-regex` names both the byte-pattern CLOS class and a macro. The macro
is the load-time compiled form of `compile-byte-regex` and accepts literal
patterns with constant keyword options.

**Macro signature:** `(byte-regex literal-pattern &rest literal-compile-options)`

### `regex-p`

**Signature:** `(regex-p object)`

Returns true when `object` is a compiled regex.

This includes both ordinary character regexes and compiled byte regexes. Use
`byte-regex-p` when the input representation must be distinguished.

### `byte-regex-p`

**Signature:** `(byte-regex-p object)`

Returns true when `object` is a compiled regex configured for octet-vector
input.

### `regex-source`

**Signature:** `(regex-source regex)`

Returns a fresh copy of the source pattern used to compile `regex`.

### `regex-group-count`

**Signature:** `(regex-group-count regex)`

Returns the number of explicit capture groups in `regex`, excluding group
zero for the whole match.

### `regex-capture-count`

**Signature:** `(regex-capture-count regex)`

Returns the total number of capture slots, including group zero.

### `regex-static-capture-count`

**Signature:** `(regex-static-capture-count regex)`

Returns the number of capture groups that participate in every possible match,
including group zero, or `nil` when that number varies.

### `regex-capture-names`

**Signature:** `(regex-capture-names regex)`

Returns a fresh vector indexed by capture number. Index zero and unnamed
captures contain `nil`; named captures contain their names.

### `regex-group-index`

**Signature:** `(regex-group-index regex name)`

Returns the numeric index of the named capture `name`, or `nil` when no
capture has that name.

### `regex-advanced-p`

**Signature:** `(regex-advanced-p regex)`

Returns true when `regex` contains constructs executed by the bounded
advanced engine.

### `regex-advanced-step-limit`

**Signature:** `(regex-advanced-step-limit regex)`

Returns the configured `:size-limit` value stored on `regex`. The compiler
stores the default as well as an explicitly supplied value on every compiled
regex. For an ordinary regex this value bounds NFA program construction; for
an advanced regex it bounds evaluation steps.

### `regex-advanced-nest-limit`

**Signature:** `(regex-advanced-nest-limit regex)`

Returns the configured `:nest-limit` value stored on `regex`. The compiler
stores the default as well as an explicitly supplied value on every compiled
regex. It bounds parser nesting and also guards advanced execution depth.

### `regex-never-newline-p`

**Signature:** `(regex-never-newline-p regex)`

Returns true when the compiled regex forbids consuming a newline.

### `regex-callout`

**Signature:** `(regex-callout regex)`

Returns the callout function associated with `regex`, or `nil` when the
pattern has no callout handler.

## Multi-pattern Compilation

### `compile-regex-set`

**Signature:** `(compile-regex-set patterns &rest options)`

Compiles a list or non-string vector of character patterns into an immutable
`regex-set`. The options are passed to each pattern compiler, and an empty
set is valid.

### `compile-byte-regex-set`

**Signature:** `(compile-byte-regex-set patterns &rest options)`

Compiles a list or non-string vector of patterns into a byte `regex-set`.
Searches on the result require octet-vector input.

### `regex-set`

`regex-set` names both the regex-set class and a macro. The macro accepts
literal string patterns and constant compile options and performs load-time
compilation.

**Macro signature:** `(regex-set &rest arguments)`

### `byte-regex-set`

`byte-regex-set` is the byte-pattern counterpart of `regex-set`.

**Macro signature:** `(byte-regex-set &rest arguments)`

### `regex-set-p`

**Signature:** `(regex-set-p object)`

Returns true when `object` is a compiled regex set, regardless of whether its
members use character or byte input.

### `byte-regex-set-p`

**Signature:** `(byte-regex-set-p object)`

Returns true when `object` is a compiled regex set configured for octet-vector
input.

### `regex-set-patterns`

**Signature:** `(regex-set-patterns regex-set)`

Returns a fresh vector containing fresh copies of the source patterns in
source order.

### `regex-set-count`

**Signature:** `(regex-set-count regex-set)`

Returns the number of source patterns in `regex-set`.

### `regex-set-empty-p`

**Signature:** `(regex-set-empty-p regex-set)`

Returns true when `regex-set` contains no patterns.

### `regex-set-matches`

**Signature:** `(regex-set-matches regex-set text &key (start 0) end timeout)`

Returns a list of source-pattern indexes that match somewhere in the selected
range, in source order. Duplicate patterns produce distinct indexes.

### `regex-set-matches-at`

**Signature:** `(regex-set-matches-at regex-set text start &key end timeout)`

Returns source-pattern indexes that match at or after the required `start`
position.

NFA-compatible members are evaluated by the merged Pike VM. Members that
require advanced ordered backtracking are retained separately and evaluated by
the bounded advanced executor, so a set containing them does not provide a
single-scan guarantee for those members.

### `regex-set-search`

**Signature:** `(regex-set-search regex-set text &key (start 0) end timeout)`

Returns two values: the source-pattern index and the corresponding
`match-result` for the earliest member match. When multiple members begin at
the same position, the lowest source-pattern index wins. Returns `nil`, `nil`
when no member matches.

### `regex-set-search-at`

**Signature:** `(regex-set-search-at regex-set text start &key end timeout)`

The required-position form of `regex-set-search`.

### `regex-set-matches-into`

**Signature:** `(regex-set-matches-into regex-set matches text &key (start 0)
end timeout)`

Clears the caller-owned bit vector `matches` and sets one bit for every
matching source-pattern index. Its length must equal `regex-set-count`.
Returns the same bit vector.

### `regex-set-match-p`

**Signature:** `(regex-set-match-p regex-set text &key (start 0) end timeout)`

Returns true when at least one pattern in `regex-set` matches the selected
range.

### `regex-set-match-at-p`

**Signature:** `(regex-set-match-at-p regex-set text start &key end timeout)`

Returns true when at least one pattern matches at or after `start`.

## Matching and Iteration

Search ranges are half-open: `:start` defaults to zero and `:end` is an
exclusive upper bound. Anchors and word boundaries continue to inspect the
original input rather than treating the selected range as a new string.

### `scan`

**Signature:** `(scan regex text &key (start 0) end timeout)`

Returns the leftmost-first `match-result` at or after `start`, or `nil`.
The input is a string for a character regex and an octet vector for a byte
regex.

### `scan-at`

**Signature:** `(scan-at regex text start &key end timeout)`

The required-position form of `scan`. It searches at or after `start`
within the selected range.

### `regex-search`

**Signature:** `(regex-search regex text &key (start 0) end timeout)`

Searches for the first match and returns the same `match-result` as `scan`.
This name is provided for callers who use search terminology rather than
scan terminology.

### `regex-search-at`

**Signature:** `(regex-search-at regex text start &key end timeout)`

The required-position form of `regex-search`.

### `captures`

**Signature:** `(captures regex text &key (start 0) end timeout)`

Returns the same capture-aware `match-result` as `scan`, including
participating and nonparticipating groups.

### `captures-at`

**Signature:** `(captures-at regex text start &key end timeout)`

The required-position form of `captures`.

### `shortest-match`

**Signature:** `(shortest-match regex text &key (start 0) end timeout)`

Returns the earliest ending match at the leftmost matching position as an
exclusive end index, or `nil`. For example, `a+` over `"aaa"` returns
index 1.

### `shortest-match-at`

**Signature:** `(shortest-match-at regex text start &key end timeout)`

Returns the shortest match end index at or after the required `start`.

### `longest-match`

**Signature:** `(longest-match regex text &key (start 0) end timeout)`

Returns a leftmost-longest `match-result`, or `nil`. This selection policy
is useful when POSIX or RE2 longest-match behavior is required.

### `match`

**Signature:** `(match pattern text &key (start 0) end timeout)`

Compiles `pattern` and performs a character search. Compile once with
`compile-regex` when the same pattern will be reused.

### `byte-match`

**Signature:** `(byte-match pattern text &key (start 0) end timeout)`

Compiles `pattern` as a byte regex and searches octet-vector `text`.

### `fuzzy-scan`

**Signature:** `(fuzzy-scan regex text &key (max-edits 1) (start 0) end timeout state-limit)`

Finds the leftmost match of a regular NFA regex while allowing bounded
insertions, deletions, and substitutions. The result has the minimum edit
distance at the selected start position; when candidates tie on distance, the
earliest ending span wins. `max-edits` defaults to one. The input type follows
the compiled regex, so byte regexes require octet vectors.
`state-limit` bounds the fuzzy state search and signals
`fuzzy-match-limit-error` when exceeded.

### `fuzzy-scan-at`

**Signature:** `(fuzzy-scan-at regex text start &key max-edits end timeout state-limit)`

The required-position form of `fuzzy-scan`.

### `fuzzy-search` and `fuzzy-search-at`

These are search-named aliases for `fuzzy-scan` and `fuzzy-scan-at` with the
same arguments and result ordering.

### `fuzzy-match`

**Signature:** `(fuzzy-match pattern text &key (max-edits 1) (start 0) end timeout state-limit)`

Compiles a character pattern when necessary and performs bounded fuzzy
matching. Use `fuzzy-scan` when reusing a compiled regex.

### `byte-fuzzy-match`

**Signature:** `(byte-fuzzy-match pattern text &key (max-edits 1) (start 0) end timeout state-limit)`

Compiles a byte pattern when necessary and performs bounded fuzzy matching on
an octet vector.

### `all-matches`

**Signature:** `(all-matches regex text &key (start 0) end timeout)`

Returns a list of non-overlapping `match-result` objects in left-to-right
order.

### `all-matches-overlapping`

**Signature:** `(all-matches-overlapping regex text &key (start 0) end timeout)`

Returns every match in left-to-right order, including matches whose spans
overlap. The next search begins one character after the previous match's
start for string input, or one octet for byte input. Empty matches are
reported at every eligible input position.

### `do-matches`

**Signature:** `(do-matches ((match regex text &key (start 0) end timeout)
&body body)`

Iterates over non-overlapping matches without first constructing a result list.
The variable `match` is bound for each body evaluation.

### `do-matches-overlapping`

**Signature:** `(do-matches-overlapping ((match regex text &key (start 0) end timeout)
&body body)`

Iterates over overlapping matches without first constructing a result list.
The variable `match` is bound for each body evaluation.

### `do-captures`

**Signature:** `(do-captures ((locations regex text &key (start 0) end timeout)
&body body)`

Iterates over captures with a reusable `capture-locations` buffer. Offset
values are valid until the next iteration.

### `do-captures-overlapping`

**Signature:** `(do-captures-overlapping ((locations regex text &key (start 0) end timeout)
&body body)`

Iterates over overlapping matches with a reusable `capture-locations` buffer.
Offset values are valid only until the next iteration.

These iteration macros consume an already materialized string or octet vector.
They are callback-style traversal APIs, not incremental chunk-stream
matchers; matches cannot carry state across separately supplied chunks.

### Chunked input

**Signatures:**

```lisp
(make-regex-stream regex &key (start 0) timeout)
(regex-stream-p object)
(regex-stream-regex stream)
(regex-stream-start stream)
(regex-stream-timeout stream)
(regex-stream-length stream)
(regex-stream-text stream)
(regex-stream-finished-p stream)
(regex-stream-feed stream chunk &key (start 0) end)
(regex-stream-finish stream &key overlapping-p callback)
(regex-stream-reset stream)
(all-stream-matches regex input-stream
                   &key (chunk-size 4096) (start 0) end timeout)
(all-stream-matches-overlapping regex input-stream
                                &key (chunk-size 4096) (start 0) end timeout)
(scan-stream regex input-stream
             &key (chunk-size 4096) (start 0) end timeout)
(do-stream-matches (match regex input-stream
                    &key (chunk-size 4096) (start 0) end timeout)
                   &body body)
(do-stream-matches-overlapping (match regex input-stream
                               &key (chunk-size 4096) (start 0) end timeout)
                              &body body)
(do-stream-captures (locations regex input-stream
                     &key (chunk-size 4096) (start 0) end timeout)
                    &body body)
(do-stream-captures-overlapping (locations regex input-stream
                                &key (chunk-size 4096) (start 0) end timeout)
                               &body body)
```

`make-regex-stream` owns an input buffer while callers append string or
octet-vector chunks. A character regex accepts strings and a byte regex
accepts octet vectors; the chunk element type must match the compiled regex.
`start` is the match-search offset. `regex-stream-feed` appends the selected
`start..end` range of each chunk and returns the stream. `regex-stream-length`
reports the number of buffered input units, while `regex-stream-text` returns
a copy suitable for resolving match offsets.

`regex-stream-finish` runs the ordinary or overlapping matcher after all
chunks have arrived. It is idempotent and returns a fresh result list on every
call. With `callback`, only results whose callback has not completed
successfully are delivered; if a callback signals, a later call resumes at
that result. Its timeout is the value supplied to `make-regex-stream` and
covers matching and callback execution. `regex-stream-finished-p` becomes
true after matching succeeds, including when a callback later signals.
`regex-stream-reset` clears the input and cached results for reuse, so match
offsets from before reset must not be used with the new `regex-stream-text`.

`all-stream-matches` and `scan-stream` perform the same operation while
reading a Common Lisp input stream. `all-stream-matches-overlapping` and the
two overlapping iteration macros use overlapping traversal. `end`, when
supplied, is an exclusive upper bound on the number of input units read;
without it, helpers read to EOF. The input stream is not closed. Their
`timeout` covers the adapter's read loop, matching, and callbacks when the
underlying stream honors the implementation's timeout interruption. An
arbitrary external or foreign blocking read may not be interruptible;
applications that need a hard deadline must supply a non-blocking or
independently bounded input stream. Iteration bodies run after the helper
returns and are therefore outside that timeout. These helpers require a
finish barrier and buffer the complete selected input; they do not claim
low-latency or bounded-memory incremental matching. The stream objects and
capture-location buffers are mutable and are not thread-safe; use one per
concurrent operation.

### Low-latency incremental input

**Signatures:**

```lisp
(make-incremental-regex-stream regex &key (start 0) timeout)
(incremental-regex-stream-p object)
(incremental-regex-stream-regex stream)
(incremental-regex-stream-start stream)
(incremental-regex-stream-timeout stream)
(incremental-regex-stream-position stream)
(incremental-regex-stream-finished-p stream)
(incremental-regex-stream-feed stream chunk &key (start 0) end)
(incremental-regex-stream-finish stream)
(incremental-regex-stream-reset stream)
```

This is the bounded-memory, low-latency API for expressions in the ordinary
NFA subset. `feed` consumes a string chunk for a character regex or an octet
vector chunk for a byte regex and returns newly finalized non-overlapping
`match-result` objects. A result is emitted only when the current NFA state
proves that the match cannot be extended by a future input unit; `finish`
resolves the final pending match and is idempotent. `position` reports the
absolute offset of the next processed input unit, beginning at `start`; it is
a character offset for string regexes and an octet offset for byte regexes.

The incremental API intentionally rejects advanced/ordered-backtracking
constructs, zero-width expressions, `\\R`, anchors, lookaround, and other
non-consuming VM operations. Unicode-aware byte regexes assemble complete
UTF-8 scalars and retain at most three trailing octets when a scalar crosses a
chunk boundary; those octets are processed when a later chunk completes the
scalar or when `finish` drains them. Use an explicit raw-byte scope such as
`(?-u:ab)` when the protocol delivers arbitrary octets rather than Unicode
scalars. Programs that mix raw-octet and Unicode-scalar consuming instructions
are rejected. The caller must retain or assemble the logical input separately
if it needs to call `match-string` on a result; the incremental stream retains
matcher state, not input bytes or characters.

### `is-match-p`

**Signature:** `(is-match-p regex text &key (start 0) end timeout)`

Returns a boolean indicating whether `regex` matches in the selected range.

### `is-match-at`

**Signature:** `(is-match-at regex text start &key end timeout)`

Returns a boolean indicating whether `regex` matches at or after `start`.

### `full-match`

**Signature:** `(full-match regex text &key (start 0) end timeout)`

Returns a `match-result` only when a matching path spans the entire selected
range, or `nil`.

### `full-match-p`

**Signature:** `(full-match-p regex text &key (start 0) end timeout)`

Boolean form of `full-match`.

### `regex-capture-locations`

**Signature:** `(regex-capture-locations regex)`

Allocates an offset buffer with one slot for every capture, including group
zero. Pass it to `scan-captures-into` or `do-captures`.

### `capture-locations`

`capture-locations` is the public structure type used for reusable capture
offset buffers. Create one with `regex-capture-locations`.

### `capture-locations-p`

**Signature:** `(capture-locations-p object)`

Returns true when `object` is a capture-locations buffer.

### `capture-locations-count`

**Signature:** `(capture-locations-count locations)`

Returns the number of capture slots in `locations`, including group zero.

### `capture-location-start`

**Signature:** `(capture-location-start locations index)`

Returns the inclusive start offset for capture `index`, or `nil` when the
capture did not participate.

### `capture-location-end`

**Signature:** `(capture-location-end locations index)`

Returns the exclusive end offset for capture `index`, or `nil` when the
capture did not participate.

### `scan-captures-into`

**Signature:** `(scan-captures-into regex locations text &key (start 0) end
timeout)`

Writes the leftmost-first capture offsets into `locations`. On success,
returns two values, the whole-match start and exclusive end; on failure,
returns two `nil` values.

### `scan-captures-into-at`

**Signature:** `(scan-captures-into-at regex locations text start &key end
timeout)`

The required-position form of `scan-captures-into`.

### `run-advanced-regex`

**Signature:** `(run-advanced-regex regex text &key (start 0) end shortest-p
longest-p never-newline-p timeout)`

Runs the bounded advanced executor for an advanced `regex` and returns a
`match-result` or `nil`. `shortest-p` and `longest-p` select the match
policy and are mutually exclusive. `timeout` uses the same positive-seconds
contract as the other matching APIs. Step and nesting limits signal
`advanced-regex-limit-error` when exceeded; a timeout signals `regex-timeout`.

## Text Transformation

### `split`

**Signature:** `(split regex text &key (start 0) end timeout)`

Splits `text` at non-overlapping matches and returns all fields.

### `split-terminator`

**Signature:** `(split-terminator regex text &key (start 0) end timeout)`

Splits at non-overlapping matches and omits an empty final field when the
delimiter ends the complete input. `start` and `end` limit delimiter discovery;
the returned fields still cover the complete original input, so reaching `end`
does not by itself omit the final field.

### `split-inclusive`

**Signature:** `(split-inclusive regex text &key (start 0) end timeout)`

Splits at non-overlapping matches while retaining each delimiter at the end of
its preceding field.

### `split-n`

**Signature:** `(split-n regex text count &key (start 0) end timeout)`

Returns at most `count` fields. A count of zero returns no fields. Empty
matches advance by one character or octet so iteration terminates.

### `replace-first`

**Signature:** `(replace-first regex text replacement &key (start 0) end
timeout)`

Replaces the first non-overlapping match in the selected range. `replacement`
can be a template string or a function of `(match-result text)`.

### `replace-all`

**Signature:** `(replace-all regex text replacement &key (start 0) end
timeout)`

Replaces every non-overlapping match in the selected range.

### `replace-n`

**Signature:** `(replace-n regex text replacement count &key (start 0) end
timeout)`

Replaces at most `count` non-overlapping matches. `count` must be a
non-negative integer.

Replacement templates use Rust-style `$$` for a literal dollar, `$0` and
numeric captures, and `$name` or `${name}` for named captures. A
function replacement receives the match result and original text. Byte regexes
use octet-vector text and replacement values.

## Match Results

### `match-result`

`match-result` is the public structure type returned by matching functions.
It records the whole-match start and end offsets, capture groups, group names,
and an optional mark.

### `match-result-p`

**Signature:** `(match-result-p object)`

Returns true when `object` is a match-result.

### `match-start`

**Signature:** `(match-start match-result)`

Returns the inclusive start offset of the whole match.

### `match-end`

**Signature:** `(match-end match-result)`

Returns the exclusive end offset of the whole match.

### `match-mark`

**Signature:** `(match-mark match-result)`

Returns the optional mark associated with the match, or `nil`.

### `match-edit-distance`

**Signature:** `(match-edit-distance match-result)`

Returns the number of fuzzy insertions, deletions, and substitutions used by
the match. Ordinary exact matches return zero.

### `match-string`

**Signature:** `(match-string match-result text)`

Returns the whole matched substring or octet subsequence from `text`.

### `match-captures`

**Signature:** `(match-captures match-result text)`

Returns a fresh vector of capture strings or octet subsequences in numeric
order. Index zero is the whole match and a nonparticipating capture is `nil`.

### `match-group-start`

**Signature:** `(match-group-start match-result index)`

Returns the inclusive start offset for a numeric or named capture, or `nil`
when that capture did not participate.

### `match-group-end`

**Signature:** `(match-group-end match-result index)`

Returns the exclusive end offset for a numeric or named capture, or `nil`
when that capture did not participate.

### `match-group-string`

**Signature:** `(match-group-string match-result index text)`

Returns the substring or octet subsequence for a numeric or named capture.
Group zero denotes the whole match.

## Conditions

### `cl-regex-kit-error`

Base condition class for errors signaled by cl-regex-kit.

### `regex-syntax-error`

Signals when a pattern cannot be parsed or compiled within the configured
resource limits.

### `regex-syntax-error-pattern`

**Signature:** `(regex-syntax-error-pattern condition)`

Returns the pattern associated with a `regex-syntax-error`.

### `regex-syntax-error-position`

**Signature:** `(regex-syntax-error-position condition)`

Returns the source position associated with a syntax error, or `nil` when no
position is available.

### `regex-syntax-error-reason`

**Signature:** `(regex-syntax-error-reason condition)`

Returns the implementation's explanation for a syntax error.

### `regex-timeout`

Signals when a matching operation exceeds its requested timeout.

### `regex-timeout-seconds`

**Signature:** `(regex-timeout-seconds condition)`

Returns the timeout value associated with a `regex-timeout`.

### `advanced-regex-limit-error`

Signals when the bounded advanced executor reaches its step or nesting limit.

### `advanced-regex-limit-kind`

**Signature:** `(advanced-regex-limit-kind condition)`

Returns the kind of advanced limit that was exceeded.

### `advanced-regex-limit`

**Signature:** `(advanced-regex-limit condition)`

Returns the configured limit value.

### `advanced-regex-limit-used`

**Signature:** `(advanced-regex-limit-used condition)`

Returns the amount of the limited resource consumed before the condition was
signaled.
