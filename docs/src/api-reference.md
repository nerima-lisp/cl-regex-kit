# API Reference

All symbols live in the `cl-regex-kit` package.

## Compilation

### `compile-regex`

```lisp
(compile-regex pattern) => regex
```

Parses and compiles `pattern` (a string) into a `regex` object ready for
`scan`. Signals `regex-syntax-error` if `pattern` cannot be parsed or exceeds
the compiler's resource limits.

### `regex`

A CLOS class representing a compiled pattern. Readers: `regex-source`,
`regex-group-count`. `regex-group-index` maps a named capture to its numeric
index, or returns `nil` when no such name exists. Use `(regex-p object)` to
test whether an object is a compiled pattern.

### `regex` macro

```lisp
(regex literal-pattern) => regex
```

Compiles a literal pattern once when its containing file is loaded. The macro
requires a string literal, making it suitable for constant patterns in hot
paths. `regex` is also the name of the CLOS class; Common Lisp keeps macro and
type namespaces separate.

### `compile-regex-set` and `regex-set`

```lisp
(compile-regex-set patterns) => regex-set
(regex-set literal-pattern*) => regex-set
```

Compiles a list or vector of patterns as an immutable multi-pattern set. The
`regex-set` macro requires string literals and compiles them once when its
containing file is loaded.

`regex-set-p` tests the type. `regex-set-patterns` returns a fresh vector of
the source patterns, so callers cannot mutate a compiled set.

## Matching

### `scan`

```lisp
(scan regex text &key (start 0)) => match-result-or-nil
```

Finds the leftmost-first match of `regex` in `text` at or after `start`.

### `is-match-p`

```lisp
(is-match-p regex text &key (start 0)) => boolean
```

Boolean form of `scan`, without constructing an application-level branch on a
`match-result`.

### `full-match-p`

```lisp
(full-match-p regex text) => boolean
```

Returns true only when the leftmost match spans all of `text`.

### `match`

```lisp
(match pattern text &key (start 0)) => match-result-or-nil
```

Convenience wrapper: `(scan (compile-regex pattern) text :start start)`. Prefer
`compile-regex` + `scan` when matching the same pattern repeatedly.

### `all-matches` and `do-matches`

```lisp
(all-matches regex text &key (start 0)) => list-of-match-result
(do-matches (result regex text &key (start 0)) form*) => nil
```

Both traverse non-overlapping matches left to right. `all-matches` collects
results; `do-matches` processes the stream incrementally without constructing a
result list.

### `regex-set-matches` and `regex-set-match-p`

```lisp
(regex-set-matches regex-set text &key (start 0)) => list-of-index
(regex-set-match-p regex-set text &key (start 0)) => boolean
```

`regex-set-matches` returns every source-pattern index that matches `text` at
or after `start`, in source order. Duplicate source patterns therefore produce
distinct indexes. `regex-set-match-p` is the boolean form when only whether at
least one member matches is needed.

## Text transformation

### `split`

```lisp
(split regex text &key (limit nil) (start 0)) => list-of-string
```

Splits `text` at non-overlapping matches. `limit` bounds the number of fields;
zero returns no fields and `nil` has no bound. Empty matches are advanced by one
character so iteration always terminates.

### `replace-first` and `replace-all`

```lisp
(replace-first regex text replacement &key (start 0)) => string
(replace-all regex text replacement &key (start 0)) => string
```

`replacement` can be a function of `(match-result text)` returning a string,
or a Rust-style template string. Template strings support `$$` for a literal
dollar, `$0` and `$1` for numeric captures, and `$name` or `${name}` for named
captures. A capture that is absent or unknown expands to the empty string.

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
