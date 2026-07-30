# Getting started

## Install

### With Nix

```nix
# flake.nix
inputs.cl-regex-kit = {
  url = "github:nerima-lisp/cl-regex-kit/v0.1.0";
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

There are no runtime dependencies. Only the test system (`cl-regex-kit/test`)
needs [cl-weave](https://github.com/nerima-lisp/cl-weave).

## Quick start

```lisp
(asdf:load-system "cl-regex-kit")

(defparameter *pattern* (cl-regex-kit:compile-regex "a.c"))
(cl-regex-kit:scan *pattern* "xx abc yy")
;; => a MATCH-RESULT spanning "abc", once the parser and matcher are implemented

;; Or, for a pattern used only once:
(cl-regex-kit:match "a.c" "abc")
```

!!! note "Current status"
    The public API and AST shape are in place, but `parse-regex`,
    `compile-to-nfa`, and `run-pike-vm` are stubs that signal an error until
    implemented. See [Roadmap](roadmap.md).

## Reading a match

```lisp
(let ((m (cl-regex-kit:scan *pattern* text)))
  (when m
    (cl-regex-kit:match-string m text)                 ; the whole match
    (cl-regex-kit:match-group-string m 1 text)))        ; capture group 1
```
