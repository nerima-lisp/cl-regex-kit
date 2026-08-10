# Conditions

`cl-regex-kit` exports six condition types. `cl-regex-kit-error` is the base
and the other five are its direct subclasses, so handling the base type
catches every error this library signals.

```lisp
(subtypep 'cl-regex-kit:regex-syntax-error 'cl-regex-kit:cl-regex-kit-error)
;; => T

(subtypep 'cl-regex-kit:cl-regex-kit-error 'error)
;; => T
```

## Base condition

### `cl-regex-kit-error`

The base condition for every error this library signals. It inherits from the
standard `error` class and defines no slots of its own. Handle it to treat any
failure from `cl-regex-kit` uniformly, and handle one of the subclasses below
to tell the failures apart.

## Syntax errors

### `regex-syntax-error`

Signalled when a pattern cannot be parsed or compiled.

| Slot | Reader | Value |
|---|---|---|
| `pattern` | `regex-syntax-error-pattern` | The pattern that failed |
| `position` | `regex-syntax-error-position` | Index into the pattern, or `nil` when the failure has no single position |
| `reason` | `regex-syntax-error-reason` | A short explanation of the failure |

`position` defaults to `nil`, so a handler accepts that value rather than
assuming an index is always present.

Anything that compiles a pattern signals it, which is more than the four
compile functions:

- `compile-regex`, `compile-byte-regex`, `compile-regex-set`, and
  `compile-byte-regex-set` signal it directly.
- `match` and `byte-match` compile the pattern they are given before matching,
  so they signal it from the matching call rather than from a separate
  compiling step.
- The literal-compiling macros `regex`, `byte-regex`, `regex-set`, and
  `byte-regex-set` expand to `load-time-value`, so a bad literal surfaces when
  the compiled file is loaded, not when the surrounding function is called.

It also covers failures found after parsing succeeds. A program that would
exceed the compile-time `:size-limit` arrives as this condition rather than a
distinct one, because the pattern is still what the caller has to change.

```lisp
(handler-case (cl-regex-kit:compile-regex "(abc")
  (cl-regex-kit:regex-syntax-error (condition)
    (list (cl-regex-kit:regex-syntax-error-pattern condition)
          (cl-regex-kit:regex-syntax-error-reason condition)
          (cl-regex-kit:regex-syntax-error-position condition))))
;; => ("(abc" "Unclosed group" 4)
```

The report combines all three slots, so an unhandled condition is already
readable without reaching for the readers:

```lisp
(handler-case (cl-regex-kit:compile-regex "(abc")
  (cl-regex-kit:regex-syntax-error (condition) (princ-to-string condition)))
;; => "Invalid regular expression \"(abc\" at position 4: Unclosed group"
```

## Time limits

### `regex-timeout`

Signalled when a matching operation exceeds its deadline.

| Slot | Reader | Value |
|---|---|---|
| `seconds` | `regex-timeout-seconds` | The deadline that was exceeded, in seconds |

Every matching entry point takes a `:timeout` argument, which is a positive
number of seconds or `nil` for no deadline. When the deadline expires, the
operation signals this condition instead of returning `nil`, so a caller tells
an abandoned match apart from an ordinary non-match.

The deadline is enforced with `sb-ext:with-timeout`, so it bounds wall-clock
time for the whole operation rather than counting work performed. A machine
under load therefore trips a deadline that the same call meets when idle.
Reach for `:timeout` when a pattern or an input comes from outside your own
code, not to bound patterns you control.

```lisp
(handler-case
    (let ((regex (cl-regex-kit:compile-regex "a+")))
      (cl-regex-kit:match-string
       (cl-regex-kit:scan regex "aaa" :timeout 0.5) "aaa"))
  (cl-regex-kit:regex-timeout (condition)
    (cl-regex-kit:regex-timeout-seconds condition)))
;; => "aaa"
```

That call finishes well inside its deadline, so it returns the matched text and
the handler never runs. The report names the deadline:

```lisp
(princ-to-string (make-condition 'cl-regex-kit:regex-timeout :seconds 0.5))
;; => "Regular expression matching exceeded 0.500 seconds"
```

## Fuzzy matching

Fuzzy operations use the regular NFA path with explicit edit and state
budgets. Advanced ordered-backtracking patterns are rejected rather than
assigned a different fuzzy meaning.

### `fuzzy-match-unsupported`

Signalled when a fuzzy operation receives a pattern that cannot run on the
regular NFA path.

| Slot | Reader | Value |
|---|---|---|
| `pattern` | `fuzzy-match-unsupported-pattern` | The unsupported pattern |
| `reason` | `fuzzy-match-unsupported-reason` | Why the pattern is outside the fuzzy dialect |

### `fuzzy-match-limit-error`

Signalled when bounded fuzzy matching reaches its state budget before it can
finish the search.

| Slot | Reader | Value |
|---|---|---|
| `kind` | `fuzzy-match-limit-kind` | The exhausted budget, currently `:states` |
| `limit` | `fuzzy-match-limit` | The configured state limit |
| `used` | `fuzzy-match-limit-used` | States explored when the limit was reached |

`max-edits` bounds the permitted Levenshtein-style insertions, deletions, and
substitutions. `state-limit` defaults to the library's bounded fuzzy search
limit and prevents an input from expanding the search without bound. The
fuzzy entry points are `fuzzy-scan`, `fuzzy-scan-at`, `fuzzy-search`,
`fuzzy-search-at`, `fuzzy-match`, and `byte-fuzzy-match`.

## Advanced executor limits

Patterns using backreferences, lookaround, or other constructs that need
ordered backtracking do not run on the Pike VM. They run on the bounded
advanced executor instead, described in
[Core concepts](../guide/core-concepts.md). `regex-advanced-p` reports which
path a compiled regex took:

```lisp
(cl-regex-kit:regex-advanced-p (cl-regex-kit:compile-regex "(?=a)a"))
;; => T
```

That executor is budgeted, and exhausting a budget signals a condition rather
than running unbounded. The budgets come from the compile-time keywords
`:size-limit` and `:nest-limit`, which `compile-regex` and `compile-byte-regex`
both accept.

### `advanced-regex-limit-error`

Signalled when the advanced executor exhausts one of its budgets.

| Slot | Reader | Value |
|---|---|---|
| `kind` | `advanced-regex-limit-kind` | Which budget was exhausted |
| `limit` | `advanced-regex-limit` | The configured value of that budget |
| `used` | `advanced-regex-limit-used` | The amount consumed when the budget was crossed |

`kind` takes one of three values:

| Kind | Budget | Configured by |
|---|---|---|
| `:steps` | Evaluation steps taken by the executor | `:size-limit` |
| `:states` | Backtracking states cloned during evaluation | `:size-limit` |
| `:nest-depth` | Nesting depth reached during evaluation | `:nest-limit` |

`:steps` and `:states` are separate counters funded from the same
`:size-limit` value, so lowering it tightens both. `used` always exceeds
`limit`, because the condition is signalled on the operation that crosses the
budget rather than on the one that reaches it.

The budgets are allocated once per matching call and are not reset as the scan
advances. A search tries each start position in turn, and every position draws
down the same step and state counters, so consumption accumulates with the
length of the subject. A long subject can therefore exhaust a budget and signal
even when the pattern is an ordinary one with no pathological backtracking. The
outcome flips from "no match" to a signalled condition as a function of input
length, not of pattern shape:

```lisp
(flet ((subject (n) (make-string n :initial-element #\a)))
  (let ((regex (cl-regex-kit:compile-regex "(?=zq)zq" :size-limit 500)))
    (list (cl-regex-kit:scan regex (subject 50))
          (handler-case (cl-regex-kit:scan regex (subject 1000))
            (cl-regex-kit:advanced-regex-limit-error (condition)
              (cl-regex-kit:advanced-regex-limit-kind condition))))))
;; => (NIL :STEPS)
```

Both calls use the same pattern and the same limit; only the subject grew. The
short subject reports no match, the long one abandons the search.

This is a bound on work, not the absence of one. The budget is a constant, so
the executor still stops after a fixed amount of work rather than running away,
and it fails closed: a long input signals promptly instead of hanging. What it
changes is the caller's contract, because "match" and "no match" are no longer
the only outcomes.

Handle `advanced-regex-limit-error` around any `scan`, `is-match-p`,
`replace-first`, `replace-all`, or `split` call that runs an advanced pattern
over input you did not choose. Do not fold the condition into the no-match
branch: `nil` means the pattern is absent from the subject, while this
condition means the search was abandoned before it could establish that. If the
distinction does not matter to your caller, collapse it deliberately rather
than by omission.

Two levers reduce how often this arises. Raise `:size-limit` at compile time to
fund a longer scan. Or test [`regex-advanced-p`](api.md#regex-advanced-p) after
compiling and keep unbounded-length input away from patterns that report true,
since patterns on the linear-time path have no such budget to exhaust.

The condition surfaces through the matching call, not the compiling call, so
handle it around `scan` rather than around `compile-regex`:

```lisp
(handler-case
    (cl-regex-kit:scan
     (cl-regex-kit:compile-regex "(a)\\1+" :size-limit 1)
     "aaaa")
  (cl-regex-kit:advanced-regex-limit-error (condition)
    (list (cl-regex-kit:advanced-regex-limit-kind condition)
          (cl-regex-kit:advanced-regex-limit condition)
          (cl-regex-kit:advanced-regex-limit-used condition))))
;; => (:STEPS 1 2)
```
