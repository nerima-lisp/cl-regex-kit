# Getting started

## Install

### With Nix

```nix
# flake.nix
inputs.cl-regex-kit = {
  url = "github:nerima-lisp/cl-regex-kit/v2.0.0";
  inputs.nixpkgs.follows = "nixpkgs";
};
```

Pin a release tag; do not follow the default branch.

### Without Nix

Put the repository where ASDF can find it (for example under
`~/quicklisp/local-projects/` or on `asdf:*central-registry*`) and load it:

```lisp
(asdf:load-system "cl-regex-kit")
```

The library depends on `cl-parser-kit` and `cl-concurrent-kit` at runtime. The
test system additionally uses [cl-weave](https://github.com/nerima-lisp/cl-weave)
and the CLI system requires [cl-cli](https://github.com/nerima-lisp/cl-cli).

## Quick start

```lisp
(asdf:load-system "cl-regex-kit")

(defparameter *pattern* (cl-regex-kit:compile-regex "a.c"))
(cl-regex-kit:scan *pattern* "xx abc yy")
;; => a MATCH-RESULT spanning "abc"

;; Or, for a pattern used only once:
(cl-regex-kit:match "a.c" "abc")
```

!!! note "Supported syntax"
    The public API supports literals, alternation, named groups, greedy and lazy
    repetition, character classes, escapes, inline flags, and line, absolute,
    and word-boundary anchors.
    See the [Roadmap](project/roadmap.md) for the exact scope and intentional
    exclusions.

## Reading a match

```lisp
(let ((m (cl-regex-kit:scan *pattern* text)))
  (when m
    (cl-regex-kit:match-string m text)                 ; the whole match
    (cl-regex-kit:match-group-string m 1 text)))        ; capture group 1
```
