# Development

The Nix flake provides the compiler, test dependencies, coverage tooling, and
documentation environment used by the project. The declared outputs include
`x86_64-linux` and `aarch64-darwin`; continuous integration gates the Linux
checks, while Darwin is the primary local development platform.

## Toolchain

Enter the development shell before running commands that need SBCL or the
Common Lisp dependencies:

```nix
nix develop
nix build
nix run .#test
nix run .#benchmark
nix fmt
nix flake check -L
```

`nix build` produces the command-line executable. `nix run .#test` runs the
test suite, `nix run .#benchmark` runs the benchmark suite, and `nix fmt`
formats Nix sources with the repository formatter. `nix flake check -L` is the
full local gate: it checks the test suite, benchmark smoke test, formatting,
coverage, and documentation.

## Tests and coverage

Tests live in `t/` and use [cl-weave](https://github.com/nerima-lisp/cl-weave).
The suite combines focused examples, shrinkable property tests, and bounded
parser fuzzing.

Run the suite directly inside the development shell when a lower-level SBCL
invocation is useful:

```console
nix develop --command sbcl --script run-tests.lisp
```

The coverage gate recompiles the handwritten production sources with
`sb-cover` and validates the generated report. To write a local HTML report,
set its output directory explicitly:

```console
nix develop --command env CL_REGEX_KIT_COVERAGE_DIRECTORY="$PWD/coverage" \
  sbcl --script run-coverage.lisp
```

The coverage thresholds and currently untested areas are documented in the
[roadmap](roadmap.md#known-gaps).

## Documentation

Build the documentation with the same strict MkDocs configuration used by the
flake:

```console
nix build .#docs --no-link --print-build-logs
```

The documentation source is under `docs/src/`. Keep public API entries aligned
with the exports in `src/package.lisp`, use package-qualified Common Lisp
examples, and run the strict build after changing pages or navigation.
