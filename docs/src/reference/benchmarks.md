# Benchmarks

The benchmark suite measures compilation and matching workloads through the
`cl-regex-kit/benchmark` system. It is a diagnostic baseline for repeated runs
in one controlled environment, not evidence of speed superiority and not a
valid cross-machine comparison.

## Running the suite

Run the benchmark application through the flake:

```console
nix run .#benchmark
```

The entry point is `run-benchmarks.lisp`, which loads the
`cl-regex-kit/benchmark` system and calls
`cl-regex-kit/benchmarks:run-benchmarks`. Invoking the script directly works
too, but only where ASDF can already resolve `cl-parser-kit` and
`cl-concurrent-kit` -- inside the development shell, for example:

```console
nix develop --command sbcl --script run-benchmarks.lisp
```

## Configuration

Every setting in the table below is read from the environment. All of them
except `CL_REGEX_KIT_BENCH_REVISION` must parse as a positive integer; the
suite signals an error rather than falling back to the default when one does
not.
`CL_REGEX_KIT_BENCH_REVISION` is a non-empty string recorded in the report
metadata.

| Variable | Default | Purpose |
| --- | ---: | --- |
| `CL_REGEX_KIT_BENCH_ITERATIONS` | `10000` | Match iterations per workload |
| `CL_REGEX_KIT_BENCH_COMPILE_ITERATIONS` | `100` | Compile iterations per workload |
| `CL_REGEX_KIT_BENCH_WARMUP` | `1000` | Warm-up iterations before measurement |
| `CL_REGEX_KIT_BENCH_SAMPLES` | `5` | Measured samples per phase and workload |
| `CL_REGEX_KIT_BENCH_SEED` | `1729` | Seed for generated benchmark input |
| `CL_REGEX_KIT_BENCH_REGEX_SET_PATTERN_COUNT` | `128` | Patterns per compiled regex set |
| `CL_REGEX_KIT_BENCH_REVISION` | `unspecified` | Revision label in report metadata |

The per-operation timeout is the one setting with no environment variable. It
is the `timeout-seconds` keyword argument to
`cl-regex-kit/benchmarks:run-benchmarks`, and `run-benchmarks.lisp` calls that
function with no arguments, so both `nix run .#benchmark` and a direct script
invocation always use its default. Changing it means calling `run-benchmarks`
yourself.

For a smaller repeatable run, override the match and compile counts and keep
the seed fixed:

```console
CL_REGEX_KIT_BENCH_ITERATIONS=5000 \
CL_REGEX_KIT_BENCH_COMPILE_ITERATIONS=50 \
CL_REGEX_KIT_BENCH_WARMUP=500 \
CL_REGEX_KIT_BENCH_SAMPLES=5 \
CL_REGEX_KIT_BENCH_SEED=1729 \
CL_REGEX_KIT_BENCH_REVISION="$(git rev-parse HEAD)" \
nix run .#benchmark
```

## Measured phases

Each workload pairs a pattern with a fixed vector of subjects and the expected
match outcome for each one. Every workload is verified against those expected
outcomes before any timing runs, so a measurement can never be taken from a
build whose matching is wrong.

Four phases run per workload:

- `:compile` -- repeated `compile-regex` of the workload pattern.
- `:compile-regex-set` with `:mode :sequential` -- `compile-regex-set` over a
  family of patterns derived from the workload, with compilation and NFA
  merging forced to a parallelism of one.
- `:compile-regex-set` with `:mode :parallel` -- the same work at the
  library's configured maximum parallelism, so the two rows are measured in the
  same process against the same pattern set.
- `:hot-match` -- repeated `is-match-p` against the workload subjects, after
  the configured warm-up.

Each phase computes a checksum alongside its timings, and the suite errors out
if that checksum differs between samples of the same phase. A run that varies
in what it computed is discarded rather than reported.

Every compile and match operation runs under a per-operation timeout, recorded
in the report metadata as `:per-operation-timeout-seconds`.

## Interpreting results

Each entry under `:results` carries the raw samples plus the median, minimum,
and maximum of both elapsed seconds and bytes consed. Runtime metadata
(implementation, version) and host metadata (machine, software type and
version) are report-level rather than per result: the suite emits them once,
under `:metadata`. Note that the allocation figure is *bytes* consed, from
`sb-ext:get-bytes-consed`, not a count of allocations; it is `nil` on
implementations other than SBCL.

Timings and allocation figures vary with garbage collection and with the host.
Compare repeated runs that used the same workload order and the same seed.
Every workload exercises this library's own `compile-regex`,
`compile-regex-set`, and `is-match-p`; the suite has no cross-implementation
arm. No number it produces is evidence of speed superiority over another regex
engine, on any host and over any number of runs.

## The flake check benchmark

`nix flake check` runs the benchmark path as a bounded smoke check
(`checks.benchmark` in `flake.nix`), with reduced iteration counts and a fixed
revision label. Its purpose is to catch a regression that breaks the benchmark
path, not to produce a comparable measurement -- read the environment
assignments in that check for the exact values it pins.
