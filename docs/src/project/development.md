# Development

The Nix flake provides the compiler, the test and benchmark dependencies, the
coverage tooling, and the documentation environment. It declares outputs for
`x86_64-linux` and `aarch64-darwin`. CI gates only the `x86_64-linux` checks;
`aarch64-darwin` is the local development platform and carries no CI gate.

## Toolchain

```console
nix develop          # SBCL with the source registry already set
nix build            # -> ./result/bin/cl-regex-kit-grep
nix run .#test       # run the test suite
nix run .#benchmark  # run the benchmark suite
nix fmt              # format Nix sources with treefmt
nix flake check -L   # the full gate
```

`nix build` produces the `cl-regex-kit-grep` command-line executable, which is
also available under its explicit name as `nix build .#cl-regex-kit-grep`.

`nix flake check` evaluates each gate as its own derivation, so they build in
parallel and cache independently. The gates are the test suite, the benchmark
smoke check, Nix formatting, the coverage report, the strict documentation
build, and a build of the delivered executable.

The development shell adds `perl`, which several validation scripts in the
flake depend on.

## Tests and coverage

Tests live in `t/` and run under
[cl-weave](https://nerima-lisp.github.io/cl-weave/) with repository-local
declarative case macros for the grep/byte matrix tests. The
suite combines
focused examples with shrinkable property tests and bounded parser fuzzing, so
a failure retains a minimal reproducing input.

`nix run .#test` supplies the dependencies the suite needs. Running the script
by hand works inside the development shell, where ASDF can already resolve
`cl-parser-kit`, `cl-concurrent-kit`, `cl-weave`, `cl-json-kit`,
`cl-codec-kit`, `cl-dataflow-kit`, and `cl-cli`:

```console
nix develop --command sbcl --script run-tests.lisp
```

The coverage gate recompiles the handwritten production sources with SBCL's
`sb-cover` and then validates the generated report, failing below 96%
expression and 92% branch coverage. Those thresholds live in
`checks.coverage` in `flake.nix`, which is the authority if this page and the
flake ever disagree.

To write an HTML report locally, set the output directory explicitly:

```console
nix develop --command env CL_REGEX_KIT_COVERAGE_DIRECTORY="$PWD/coverage" \
  sbcl --script run-coverage.lisp
```

That writes `cover-index.html` and the per-file reports under the directory
you named. Why the gate sits below 100%, and which areas remain uncovered, is
recorded in the [roadmap](roadmap.md#known-gaps).

## Benchmarks

The benchmark suite and its environment variables are documented in
[Benchmarks](../reference/benchmarks.md).

## Documentation

The documentation source is under `docs/src/`, configured by
`docs/mkdocs.yml`. Build it with the same strict MkDocs configuration the
flake uses:

```console
nix build .#docs --no-link --print-build-logs
```

The build runs `mkdocs build --strict`, so a broken internal link or a page
missing from `nav:` fails it. `mkdocs.yml` also sets `strict: true`, which
applies the same gate to `mkdocs serve`; pass `--no-strict` to that command
while a page is mid-edit.

When changing documentation, keep the public API entries aligned with the
exports in `src/package.lisp`, use package-qualified Common Lisp in runnable
examples, and add every new page to `nav:`.

## Releases

The `:version` in `cl-regex-kit.asd` is the single source of truth for the
package release. A release tag must use the corresponding `v<version>` form.
Before tagging, run the full flake check:

```console
nix flake check --print-build-logs
```

Push `main` and the matching tag to start the release workflow. The workflow
verifies the tag against the ASDF version and creates a draft GitHub Release
after the checks pass. Review the generated artifact and publish the draft with
release notes once the workflow is green; GitHub Release notes are the
project's changelog.
