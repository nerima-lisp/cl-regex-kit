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
      url = "github:nerima-lisp/cl-nix-forge/v0.4.0";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    cl-weave = {
      url = "github:nerima-lisp/cl-weave/v1.1.0";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    # The parser (regex-tokenizer.lisp/regex-grammar.lisp) is built on
    # cl-parser-kit's token/span/tokenizer model, so unlike cl-weave this is
    # a real runtime dependency of the `cl-regex-kit` system itself, not
    # `cl-regex-kit/test` -- see `lispDependencies` below and
    # `cl-regex-kit.asd`'s `:depends-on`.
    cl-parser-kit = {
      url = "github:nerima-lisp/cl-parser-kit/v1.0.1";
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
      cl-parser-kit,
      treefmt-nix,
    }:
    let
      lib = nixpkgs.lib;

      # Only what is verified: x86_64-linux by CI, aarch64-darwin by the
      # maintainer's `nix flake check` on every local run. Do NOT pass
      # --all-systems in ci.yml: the runner is x86_64-linux and would try to
      # evaluate the darwin derivations and fail.
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
      # `:depends-on ("cl-parser-kit")` -- so it is a `lispDependencies`
      # edge, resolved for every build, not gated behind `doCheck` the way
      # `lispCheckDependencies` is.
      # cl-parser-kit is itself an `mkPackageFlake` (cl-nix-forge) package,
      # not a foreign one -- per cl-nix-forge's own dependency docs, "once a
      # sibling repository's own flake exposes its package, it is an
      # ordinary `lispDependencies` entry", passed as-is rather than through
      # `cl.fromDerivation` (which is for packages this library did not
      # build and cannot assume anything about, e.g. whether they carry
      # fasls a consumer could reuse without recompiling into their own,
      # read-only, store path).
      lispDependencies = ctx: [ cl-parser-kit.packages.${ctx.system}.cl-parser-kit ];

      # cl-weave is a test-only ASDF dependency (`cl-regex-kit/test` depends
      # on it; the production system's only dependency is cl-parser-kit,
      # above). `lispCheckDependencies` is resolved only under `doCheck`, so
      # the plain library package doesn't drag in the test framework,
      # matching `cl-regex-kit.asd`'s `cl-regex-kit/test :depends-on`.
      lispCheckDependencies = ctx: [
        (ctx.cl.fromDerivation { drv = cl-weave.packages.${ctx.system}.cl-weave; })
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

      devShellPackages = ctx: [ ctx.pkgs.perl ];

      # Granularity lives here, NOT in extra GitHub Actions jobs: `nix flake
      # check` evaluates each attribute as its own derivation, in parallel,
      # with build caching.
      extraOutputs = ctx: {
        checks.coverage = ctx.cl.mkCommandCheck {
          # `.enableCheck`, not `ctx.package`: the coverage run loads
          # `cl-regex-kit/test`, which needs `lispCheckDependencies` (cl-weave)
          # on the resolved registry, and only the check-enabled derivation
          # carries that.
          drv = ctx.package.enableCheck;
          name = "cl-regex-kit-coverage";
          timeoutSeconds = 120;
          nativeBuildInputs = [ ctx.pkgs.perl ];
          command = [
            "${ctx.pkgs.sbcl}/bin/sbcl"
            "--script"
            "run-coverage.lisp"
          ];
          artifacts = [ "coverage/" ];
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
                die sprintf("expression coverage %.2f%% is below 90%%\n",
                            $expression_percent)
                  if $expression_percent < 90;
                die sprintf("branch coverage %.2f%% is below 85%%\n",
                            $branch_percent)
                  if $branch_percent < 85;
              ' coverage/cover-index.html
            ''
          ];
        };
      };
    };
}
