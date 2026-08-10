# Conditions

`cl-regex-kit` signals conditions for invalid patterns, ordinary match
timeouts, and resource limits in the advanced executor. All conditions inherit
from `cl-regex-kit-error`.

## Base condition

### `cl-regex-kit-error`

The base condition for errors signalled by the library. It inherits from
Common Lisp `error` and has no additional slots.

## Syntax errors

### `regex-syntax-error`

`compile-regex` and `compile-byte-regex` signal this condition when a pattern
cannot be parsed or compiled. The condition retains the original pattern, an
optional source position, and a human-readable reason.

### `regex-syntax-error-pattern`

Return the pattern associated with a `regex-syntax-error`.

### `regex-syntax-error-position`

Return the source position associated with a `regex-syntax-error`, or `nil`
when the compiler cannot associate the error with one position.

### `regex-syntax-error-reason`

Return the explanatory reason associated with a `regex-syntax-error`.

## Time limits

### `regex-timeout`

Matching operations signal this condition when their `:timeout` limit is
exceeded. Handle it with `handler-bind` or `handler-case` when a caller needs
to distinguish an incomplete match from an ordinary non-match.

### `regex-timeout-seconds`

Return the timeout limit, in seconds, stored in a `regex-timeout` condition.

## Advanced executor limits

### `advanced-regex-limit-error`

`run-advanced-regex` signals this condition when the advanced executor reaches
its configured step or nesting limit. The condition records which limit was
reached, its configured value, and the amount used.

### `advanced-regex-limit-kind`

Return the kind of limit reached by an `advanced-regex-limit-error`.

### `advanced-regex-limit`

Return the configured limit stored in an `advanced-regex-limit-error`.

### `advanced-regex-limit-used`

Return the amount consumed when an `advanced-regex-limit-error` was signalled.

## Fuzzy matching limits

### `fuzzy-match-unsupported`

`fuzzy-scan` signals this condition when the regex uses advanced
ordered-backtracking constructs. Bounded fuzzy matching currently applies to
regular NFA regexes only. The condition retains the source pattern and a
human-readable reason.

### `fuzzy-match-unsupported-pattern`

Return the source pattern associated with a `fuzzy-match-unsupported`.

### `fuzzy-match-unsupported-reason`

Return the explanatory reason associated with a `fuzzy-match-unsupported`.

### `fuzzy-match-limit-error`

`fuzzy-scan` signals this condition when its bounded fuzzy state search
reaches `state-limit`. The condition records the limit kind, configured
limit, and number of states already used.

### `fuzzy-match-limit-kind`

Return the kind of limit reached by a `fuzzy-match-limit-error`.

### `fuzzy-match-limit`

Return the configured limit stored in a `fuzzy-match-limit-error`.

### `fuzzy-match-limit-used`

Return the number of states consumed when a `fuzzy-match-limit-error` was
signalled.

```lisp
(handler-case
    (scan (compile-regex "(a+)+$") "aaaaaaaa" :timeout 0.01)
  (regex-timeout (condition)
    (format t "Timed out after ~,3F seconds~%"
            (regex-timeout-seconds condition))))
```
