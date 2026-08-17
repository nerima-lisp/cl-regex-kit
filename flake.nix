{
  description = "A from-scratch regular expression engine for Common Lisp, built on Thompson NFA construction and Pike's VM for linear-time matching";

  inputs = {
    # nixos-unstable, not nixpkgs-unstable: it advances only after the NixOS
    # release tests pass, so it is less likely to land a broken build.
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";

    # Sibling packages are ALWAYS pinned to a release tag. A bare
    # `github:nerima-lisp/cl-nix-forge` follows that repo's default branch,
    # which means an upstream push to main breaks this repo's CI without
    # warning.
    #
    # `inputs.nixpkgs.follows` is mandatory on every input: without it each one
    # drags in its own nixpkgs, inflating flake.lock and rebuilding the same
    # derivations.
    #
    # The org flake preset. Everything this file used to spell out by hand --
    # the `.asd` version extraction, `forAllSystems`, the treefmt eval wired to
    # both `formatter` and `checks.formatting`, the mkdocs package plus its
    # check, the run-tests.lisp gate with its timeout, the `apps.test`/
    # `apps.default` pair, and the devShell -- is one `mkPackageFlake` call
    # below.
    cl-nix-forge = {
      url = "github:nerima-lisp/cl-nix-forge/v0.5.0";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    cl-weave = {
      url = "github:nerima-lisp/cl-weave/v1.3.0";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    cl-json-kit = {
      url = "github:nerima-lisp/cl-json-kit/v1.2.0";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    cl-codec-kit = {
      url = "github:nerima-lisp/cl-codec-kit/v0.5.0";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    cl-prolog-kit = {
      url = "github:nerima-lisp/cl-prolog-kit/v1.5.0";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    cl-dataflow-kit = {
      url = "github:nerima-lisp/cl-dataflow-kit/v1.2.0";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    # The parser (regex-tokenizer.lisp/regex-grammar.lisp) is built on
    # cl-parser-kit's token/span/tokenizer model, so unlike cl-weave this is
    # a real runtime dependency of the `cl-regex-kit` system itself, not
    # `cl-regex-kit/test` -- see `lispDependencies` below and
    # `cl-regex-kit.asd`'s `:depends-on`.
    cl-parser-kit = {
      url = "github:nerima-lisp/cl-parser-kit/v1.1.1";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    # Regex-set compilation now uses cl-concurrent-kit for bounded parallel
    # compilation of independent member regexes before the merge pass.
    cl-concurrent-kit = {
      url = "github:nerima-lisp/cl-concurrent-kit/v0.6.1";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    # `cl-regex-kit/cli` (cli/main.lisp) -- the cl-regex-kit-grep example
    # executable -- depends on it directly for argument parsing, with no
    # adapter layer.
    cl-cli = {
      url = "github:nerima-lisp/cl-cli/v1.3.0";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    # This repository already carries paredit.toml, so the dev shell should
    # expose the org's formatter/linter instead of requiring ad-hoc installs.
    paredit-cli = {
      url = "github:nerima-lisp/paredit-cli/v1.6.0";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    treefmt-nix = {
      url = "github:numtide/treefmt-nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs =
    {
      self,
      nixpkgs,
      cl-nix-forge,
      cl-weave,
      cl-json-kit,
      cl-codec-kit,
      cl-prolog-kit,
      cl-dataflow-kit,
      cl-parser-kit,
      cl-concurrent-kit,
      cl-cli,
      paredit-cli,
      treefmt-nix,
    }:
    let
      lib = nixpkgs.lib;

      # x86_64-linux is what CI gates; aarch64-darwin is the development
      # machine. Every per-system output -- packages, checks, apps AND devShells
      # -- comes from this one list, so leaving aarch64-darwin out takes `nix
      # build` and `nix develop` off the development machine as well. That trade
      # was made on 2026-08-01 and reverted on 2026-08-02; aarch64-darwin carries
      # no CI gate, which PACKAGE_STANDARD.md's "systems" section accepts
      # explicitly. aarch64-linux and x86_64-darwin are nobody's verification and
      # are not declared.
      systems = [
        "x86_64-linux"
        "aarch64-darwin"
      ];

      meta = {
        description = "A from-scratch regular expression engine for Common Lisp, built on Thompson NFA construction and Pike's VM for linear-time matching";
        homepage = "https://github.com/nerima-lisp/cl-regex-kit";
        license = lib.licenses.mit;
        platforms = lib.platforms.unix;
      };

      # `cl-regex-kit/cli` delivered as a binary. Lifted out of `extraOutputs`
      # into this `let` because `overrideOutputs` publishes it too, as
      # `packages.default`/`apps.default`, so that a bare `nix build` in this
      # checkout produces `./result/bin/cl-regex-kit-grep`. Same derivation
      # from both call sites.
      #
      # `programPath` is not optional. It defaults to `lispSystem`, which is
      # right only when the system name, its `:build-pathname` and its
      # directory all coincide; `cl-regex-kit/cli` sets `:pathname "cli"` and
      # `:build-pathname "cl-regex-kit-grep"`, and ASDF's `program-op` resolves
      # the latter against the former, so the program is written to
      # `cli/cl-regex-kit-grep` and nothing else finds it. Without this,
      # `mkExecutable` looks for `$out/cl-regex-kit/cli` and fails the build
      # outright (cl-nix-forge lib/batteries/app.nix, `resolvedProgramPath`).
      grepExecutable =
        ctx:
        ctx.cl.mkExecutable {
          programPath = "cli/cl-regex-kit-grep";
          args = {
            pname = "cl-regex-kit-grep";
            lispSystem = "cl-regex-kit/cli";
            version = ctx.cl.fromAsdSystem ./cl-regex-kit.asd;
            src = ctx.cl.mkLispSource { root = ./.; };
            # cl-regex-kit itself is a sibling lispDependencies edge, exactly
            # like cl-parser-kit is for the main package below; cl-cli is
            # the one new dependency this executable adds. `ctx.package`
            # carries the runtime dependencies transitively -- `lispDerivation` resolves
            # the registry through the whole closure -- so naming it here
            # would be a duplicate, not an addition.
            lispDependencies = [
              ctx.package
              cl-cli.packages.${ctx.system}.cl-cli
            ];
            meta = meta // {
              description = "cl-regex-kit-grep: a small grep built directly on cl-regex-kit";
              mainProgram = "cl-regex-kit-grep";
            };
          };
        };

      # cl-json-kit's upstream flake intentionally exports `packages` only for
      # x86_64-linux, so consuming `cl-json-kit.packages.${ctx.system}` makes
      # aarch64-darwin evaluation fail before any benchmark or test can run.
      # The source tree itself is portable and has no Lisp dependencies of its
      # own (`cl-json-kit.asd`), so this repository builds the ASDF system
      # locally from the pinned input source instead of consuming the upstream
      # package output directly.
      toPath = x: /. + builtins.unsafeDiscardStringContext "${x}";
      clJsonKitSource = toPath cl-json-kit;
      clCodecKitSource = toPath cl-codec-kit;
      clDataflowSource = toPath cl-dataflow-kit;

      clJsonKitPackage =
        ctx:
        ctx.cl.lispDerivation {
          lispSystem = "cl-json-kit";
          version = ctx.cl.fromAsdSystem "${clJsonKitSource}/cl-json-kit.asd";
          src = ctx.cl.mkLispSource { root = clJsonKitSource; };
        };

      clCodecKitPackage =
        ctx:
        ctx.cl.lispDerivation {
          lispSystem = "cl-codec-kit";
          version = ctx.cl.fromAsdSystem "${clCodecKitSource}/cl-codec-kit.asd";
          src = ctx.cl.mkLispSource { root = clCodecKitSource; };
        };

      clDataflowPackage =
        ctx:
        ctx.cl.lispDerivation {
          lispSystem = "cl-dataflow-kit";
          version = ctx.cl.fromAsdSystem "${clDataflowSource}/cl-dataflow-kit.asd";
          src = ctx.cl.mkLispSource { root = clDataflowSource; };
          lispDependencies = [
            cl-concurrent-kit.packages.${ctx.system}.cl-concurrent-kit
            cl-prolog-kit.packages.${ctx.system}.cl-prolog-kit
          ];
        };
    in
    # `mkPackageFlake` spans systems -- it obtains a `pkgs` and its own
    # cl-nix-forge instance per entry in `systems` -- so the per-system `lib`
    # instance this function is *taken from* contributes nothing but the
    # function itself.
    cl-nix-forge.lib.${builtins.head systems}.mkPackageFlake {
      inherit
        self
        systems
        nixpkgs
        meta
        ;
      pname = "cl-regex-kit";

      # Single source of truth for the package version: the `:version` form in
      # cl-regex-kit.asd. A release only ever edits the .asd file and every Nix
      # package (default + docs) follows automatically.
      asd = ./cl-regex-kit.asd;

      # Required, and it must be a path literal rather than `self`: a flake's
      # `self` is string-like and `lib.fileset` refuses it. `./.` is the same
      # directory as a path.
      root = ./.;

      # cl-parser-kit is `cl-regex-kit` itself's parser toolkit -- see
      # regex-tokenizer.lisp/regex-grammar.lisp and `cl-regex-kit.asd`'s
      # `:depends-on`. cl-concurrent-kit is likewise a runtime dependency now
      # that regex-set compilation uses its bounded executor map. Both are
      # `lispDependencies` edges, resolved for every build, not gated behind
      # `doCheck` the way `lispCheckDependencies` is.
      # These sibling flakes are themselves `mkPackageFlake` packages, not
      # foreign ones -- per cl-nix-forge's own dependency docs, "once a
      # sibling repository's own flake exposes its package, it is an
      # ordinary `lispDependencies` entry", passed as-is rather than through
      # `cl.fromDerivation` (which is for packages this library did not
      # build and cannot assume anything about, e.g. whether they carry
      # fasls a consumer could reuse without recompiling into their own,
      # read-only, store path).
      lispDependencies = ctx: [
        cl-parser-kit.packages.${ctx.system}.cl-parser-kit
        cl-concurrent-kit.packages.${ctx.system}.cl-concurrent-kit
      ];

      # cl-weave, cl-codec-kit, cl-dataflow-kit, cl-json-kit, and cl-cli are
      # test-only ASDF
      # dependencies (the production system's runtime dependencies are
      # cl-parser-kit and cl-concurrent-kit, above): `cl-regex-kit.asd`'s
      # `cl-regex-kit/test` depends directly on cl-weave, cl-json-kit, and
      # cl-codec-kit, and
      # transitively on cl-cli through its own `:depends-on ("cl-regex-kit"
      # "cl-regex-kit/cli" "cl-weave")`, since
      # it also depends on `cl-regex-kit/benchmark`, which in turn brings
      # in cl-dataflow-kit and cl-json-kit,
      # `cl-regex-kit/cli` (cli/main.lisp, the
      # cl-regex-kit-grep example) is itself built on cl-cli -- see
      # `packages.cl-regex-kit-grep` below for that same edge on the
      # production side. `lispCheckDependencies` is resolved only under
      # `doCheck`, so the plain library package doesn't drag in the test
      # framework or the CLI's own dependency.
      # cl-cli, like cl-parser-kit above, is itself an `mkPackageFlake`
      # sibling, so it is passed as-is rather than through `cl.fromDerivation`.
      lispCheckDependencies = ctx: [
        (ctx.cl.fromDerivation { drv = cl-weave.packages.${ctx.system}.cl-weave; })
        (clJsonKitPackage ctx)
        (clCodecKitPackage ctx)
        (clDataflowPackage ctx)
        cl-cli.packages.${ctx.system}.cl-cli
      ];

      # `checks.default` and `apps.test` both drive run-tests.lisp (the
      # default `runner`) from this one number, so the command a contributor
      # runs by hand and the gate CI runs cannot drift apart.
      timeoutSeconds = 120;
      killAfterSeconds = 30;

      # Rooted at the repository so mkdocs.yml's relative paths resolve the
      # same way they do when run by hand. `mkDocsSite` builds with --strict,
      # so a broken link or a page missing from the nav fails the build.
      docs = {
        root = ./.;
        fileset = lib.fileset.unions [
          ./docs/mkdocs.yml
          ./docs/src
        ];
        mkdocsYmlName = "docs/mkdocs.yml";
      };

      # One treefmt evaluation drives `nix fmt` and the `checks.formatting`
      # gate, so the formatter and the CI gate can never disagree about what
      # "formatted" means. Scope stays Nix-only: nixfmt (RFC-style) is a
      # zero-footgun, low-diff formatter, whereas a YAML formatter mangles the
      # GitHub Actions `on:` key and reformatting Markdown would churn the
      # whole docs tree.
      treefmt.evalModule = treefmt-nix.lib.evalModule;

      devShellPackages = ctx: [
        ctx.pkgs.perl
        paredit-cli.packages.${ctx.system}.default
      ];

      # `packages.default`/`apps.default` are the DELIVERED BINARY, so that a
      # bare `nix build` in this checkout produces
      # `./result/bin/cl-regex-kit-grep` and `nix run .` runs it, the same way
      # it does in every other repository in the org that ships a command.
      # `packages.cl-regex-kit` -- the entry a downstream `lispDependencies`
      # edge reads, and what `overlays.default`'s attribute is named after --
      # is untouched and still the library.
      #
      # Done here rather than through `mkPackageFlake`'s own `executable`
      # argument, which computes the delivery's `args` from the LIBRARY's:
      # `cl-regex-kit/cli` is a different ASDF system with a different
      # dependency set (it adds cl-cli) and a `programPath` of its own, so it
      # needs the full `mkExecutable` call that `grepExecutable` already is.
      overrideOutputs = ctx: {
        packages.default = grepExecutable ctx;
        apps.default = ctx.cl.mkApp { drv = grepExecutable ctx; };
      };

      # Granularity lives here, NOT in extra GitHub Actions jobs: `nix flake
      # check` evaluates each attribute as its own derivation, in parallel,
      # with build caching.
      extraOutputs =
        ctx:
        let
          benchmark = ctx.cl.mkTestApp {
            pname = "cl-regex-kit-benchmark";
            src = ./.;
            runner = "run-benchmarks.lisp";
            lispDependencies = [
              ctx.package
              (clDataflowPackage ctx)
              (clJsonKitPackage ctx)
            ];
            timeoutSeconds = 120;
            killAfterSeconds = 30;
            description = "Run the cl-regex-kit benchmark suite";
          };
        in
        {
          apps.benchmark = benchmark;

          # The NAMED spelling of what `overrideOutputs` also publishes as
          # `packages.default`. Both are kept rather than collapsed into one:
          # `nix build .#cl-regex-kit-grep` says which binary it builds, which
          # a bare `nix build` cannot. Same derivation, so the duplicate costs
          # nothing.
          packages.cl-regex-kit-grep = grepExecutable ctx;

          # The delivery is the only artifact here that ASDF's `program-op`
          # actually writes a file for, and `mkExecutable` fails the build if
          # that file is missing -- so putting it in `checks` is what makes a
          # wrong `programPath` a red CI run rather than a broken output nobody
          # builds. It was exactly that: this executable was unreachable from
          # `nix flake check` and its `programPath` was wrong.
          checks.cl-regex-kit-grep = grepExecutable ctx;

          checks.coverage = ctx.cl.mkCommandCheck {
            # `.enableCheck`, not `ctx.package`: the coverage run loads
            # `cl-regex-kit/test`, which needs `lispCheckDependencies` (cl-weave)
            # on the resolved registry, and only the check-enabled derivation
            # carries that.
            drv = ctx.package.enableCheck;
            name = "cl-regex-kit-coverage";
            timeoutSeconds = 120;
            killAfterSeconds = 30;
            nativeBuildInputs = [ ctx.pkgs.perl ];
            command = [
              "${ctx.pkgs.sbcl}/bin/sbcl"
              "--script"
              "run-coverage.lisp"
            ];
            artifacts = [ "coverage/" ];
            # Gate at 96%/92%, not 100%/100%. These are release thresholds:
            # they keep the report sensitive to reachable regressions while
            # accounting for SB-COVER structural blind spots. SB-COVER does
            # not mark `in-package`, value-less `defvar`/`defconstant`,
            # `defmacro`/`defclass` bodies, `defparameter` data literals, or
            # `&key` defaults as executed, and defensive catch-all errors
            # should remain even when the current enum is exhaustive. The
            # generated report is still parsed as non-empty data below; a
            # suite that passes while coverage drops below either threshold
            # must fail this check.
            validationCommands = [
              ''
                test -s coverage/cover-index.html
                perl -0777 -ne '
                  my ($expressions_covered, $expressions_total,
                      $branches_covered, $branches_total) = (0, 0, 0, 0);
                  while (m{
                    <tr\ class=\x27(?:odd|even)\x27>
                    <td\ class=\x27text-cell\x27>.*?</td>
                    <td>(\d+)</td><td>(\d+)</td><td>[^<]*</td>
                    <td>(\d+|-)</td><td>(\d+|-)</td>
                  }gsx) {
                    $expressions_covered += $1;
                    $expressions_total += $2;
                    if ($3 ne "-") {
                      $branches_covered += $3;
                      $branches_total += $4;
                    }
                  }
                  die "no source coverage rows found\n"
                    unless $expressions_total && $branches_total;
                  my $expression_percent =
                    100 * $expressions_covered / $expressions_total;
                  my $branch_percent = 100 * $branches_covered / $branches_total;
                  printf "Coverage: expressions %d/%d (%.2f%%), branches %d/%d (%.2f%%)\n",
                    $expressions_covered, $expressions_total, $expression_percent,
                    $branches_covered, $branches_total, $branch_percent;
                  die sprintf("expression coverage %.2f%% is below the 96%% gate\n",
                              $expression_percent)
                    if $expression_percent < 96;
                  die sprintf("branch coverage %.2f%% is below the 92%% gate\n",
                              $branch_percent)
                    if $branch_percent < 92;
                ' coverage/cover-index.html
              ''
            ];
          };

          checks.benchmark = ctx.cl.mkCommandCheck {
            drv = ctx.package;
            name = "cl-regex-kit-benchmark";
            timeoutSeconds = 120;
            killAfterSeconds = 30;
            command = [
              "${ctx.pkgs.coreutils}/bin/env"
              "CL_REGEX_KIT_BENCH_ITERATIONS=100"
              "CL_REGEX_KIT_BENCH_COMPILE_ITERATIONS=10"
              "CL_REGEX_KIT_BENCH_WARMUP=10"
              "CL_REGEX_KIT_BENCH_SAMPLES=1"
              "CL_REGEX_KIT_BENCH_SEED=1729"
              "CL_REGEX_KIT_BENCH_REVISION=nix-check"
              benchmark.program
            ];
          };
        };
    };
}
