# Benchmarks

The benchmark suite measures compilation and matching workloads through the
`cl-regex-kit/benchmark` system. It is a diagnostic baseline for repeated runs
in a controlled environment, not a cross-machine performance claim.

## Running the suite

Run the benchmark application through the flake or invoke its script directly:

```console
nix run .#benchmark
sbcl --script run-benchmarks.lisp
```

The entry point is `run-benchmarks.lisp`. It loads the
`cl-regex-kit/benchmark` system and accepts positive integer configuration
values from the environment.

## Configuration

| Variable | Default | Purpose |
| --- | ---: | --- |
| `CL_REGEX_KIT_BENCH_ITERATIONS` | `10000` | Match iterations per workload |
| `CL_REGEX_KIT_BENCH_COMPILE_ITERATIONS` | `100` | Compile iterations per workload |
| `CL_REGEX_KIT_BENCH_WARMUP` | `1000` | Warm-up iterations before measurement |
| `CL_REGEX_KIT_BENCH_SAMPLES` | `5` | Measured samples per phase and workload |
| `CL_REGEX_KIT_BENCH_SEED` | `1729` | Seed for generated benchmark input |
| `CL_REGEX_KIT_BENCH_REVISION` | `unspecified` | Revision label in report metadata |

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

## Interpreting results

Each result includes raw samples, median, minimum, and maximum elapsed time,
allocation count, runtime metadata, and host metadata. Timings and allocation
counts vary with garbage collection and the host environment. Compare repeated
runs with the same workload order and seed; do not treat one run as evidence
that the library is faster than another implementation on a different host.

The `nix flake check` benchmark is a bounded smoke check with reduced
iteration counts and fixed metadata. Its purpose is to catch regressions in the
benchmark path while keeping the development gate finite.
