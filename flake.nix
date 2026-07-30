{
  description = "A from-scratch regular expression engine for Common Lisp, built on Thompson NFA construction and Pike's VM for linear-time matching";

  inputs = {
    # nixos-unstable, not nixpkgs-unstable: it advances only after the NixOS
    # release tests pass, so it is less likely to land a broken build.
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";

    # Sibling packages are ALWAYS pinned to a release tag. A bare
    # `github:nerima-lisp/cl-weave` follows that repo's default branch, which
    # means an upstream push to main breaks this repo's CI without warning.
    #
    # `inputs.nixpkgs.follows` is mandatory: without it each input drags in its
    # own nixpkgs, inflating flake.lock and rebuilding the same derivations.
    cl-weave = {
      url = "github:nerima-lisp/cl-weave/v1.0.1";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    treefmt-nix = {
      url = "github:numtide/treefmt-nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs =
    inputs@{
      self,
      nixpkgs,
      cl-weave,
      treefmt-nix,
      ...
    }:
    let
      # The flake never advertises a platform nobody verifies. Both of these
      # are verified: x86_64-linux by CI, aarch64-darwin by the maintainer's
      # `nix flake check` on every local run.
      #
      # Do NOT pass --all-systems in ci.yml: the runner is x86_64-linux and
      # would try to evaluate the darwin derivations and fail.
      systems = [
        "x86_64-linux"
        "aarch64-darwin"
      ];
      forAllSystems = nixpkgs.lib.genAttrs systems;

      # CL_SOURCE_REGISTRY for the test/dev environment.
      sourceRegistry = "${cl-weave}//:${self}//";

      # Single source of truth for the package version: the `:version` form in
      # cl-regex-kit.asd. A release only ever edits the .asd file and every Nix
      # package (default + docs) follows automatically. Nix regexes are
      # whole-string anchored and `.` never spans newlines, so the version is
      # extracted line-by-line rather than with one multi-line match.
      version =
        let
          lines = nixpkgs.lib.splitString "\n" (builtins.readFile ./cl-regex-kit.asd);
          versionLine = builtins.head (
            builtins.filter (line: builtins.match "[[:space:]]*:version \"[^\"]*\"" line != null) lines
          );
        in
        builtins.head (builtins.match "[[:space:]]*:version \"([^\"]*)\"" versionLine);

      # treefmt drives `nix fmt` and the `checks.<system>.formatting` gate.
      # Scope is Nix only: nixfmt (RFC-style) is a zero-footgun, low-diff
      # formatter, whereas YAML formatters mangle the GitHub Actions `on:`
      # key and Markdown reformatting would churn the whole docs tree.
      treefmtEval = forAllSystems (
        system:
        treefmt-nix.lib.evalModule nixpkgs.legacyPackages.${system} {
          projectRootFile = "flake.nix";
          programs.nixfmt.enable = true;
        }
      );
    in
    {
      packages = forAllSystems (
        system:
        let
          pkgs = nixpkgs.legacyPackages.${system};
        in
        rec {
          cl-regex-kit = pkgs.sbcl.buildASDFSystem {
            pname = "cl-regex-kit";
            inherit version;
            src = self;
            systems = [ "cl-regex-kit" ];
          };
          default = cl-regex-kit;

          # Rendered documentation site (Material for MkDocs).
          # Build fully offline: Material for MkDocs bundles all of its assets,
          # so no network access is required inside the Nix sandbox. --strict
          # promotes broken links and unlisted pages to build failures.
          docs = pkgs.stdenvNoCC.mkDerivation {
            pname = "cl-regex-kit-docs";
            inherit version;
            src = pkgs.lib.fileset.toSource {
              root = ./docs;
              fileset = pkgs.lib.fileset.unions [
                ./docs/mkdocs.yml
                ./docs/src
              ];
            };
            nativeBuildInputs = [ pkgs.python3Packages.mkdocs-material ];
            buildPhase = ''
              runHook preBuild
              mkdocs build --strict --config-file mkdocs.yml --site-dir "$out"
              runHook postBuild
            '';
            dontInstall = true;
            meta = {
              description = "Rendered MkDocs (Material) documentation for cl-regex-kit";
              homepage = "https://github.com/nerima-lisp/cl-regex-kit";
              license = pkgs.lib.licenses.mit;
            };
          };
        }
      );

      # `nix fmt` entry point.
      formatter = forAllSystems (system: treefmtEval.${system}.config.build.wrapper);

      # Granularity lives here, NOT in extra GitHub Actions jobs: `nix flake
      # check` evaluates each attribute as its own derivation, in parallel,
      # with build caching.
      checks = forAllSystems (
        system:
        let
          pkgs = nixpkgs.legacyPackages.${system};
        in
        {
          default =
            pkgs.runCommand "cl-regex-kit-tests"
              {
                nativeBuildInputs = [
                  pkgs.sbcl
                  pkgs.coreutils
                ];
                CL_SOURCE_REGISTRY = sourceRegistry;
              }
              ''
                export HOME="$TMPDIR/home"
                mkdir -p "$HOME" "$out"
                timeout 120 sbcl --script ${self}/run-tests.lisp
                touch "$out/passed"
              '';

          coverage =
            pkgs.runCommand "cl-regex-kit-coverage"
              {
                nativeBuildInputs = [
                  pkgs.sbcl
                  pkgs.coreutils
                  pkgs.perl
                ];
                CL_SOURCE_REGISTRY = sourceRegistry;
              }
              ''
                export HOME="$TMPDIR/home"
                export CL_REGEX_KIT_ROOT="${self}"
                export CL_REGEX_KIT_COVERAGE_DIRECTORY="$out"
                mkdir -p "$HOME"
                timeout 120 sbcl --script ${./run-coverage.lisp}
                test -f "$out/cover-index.html"
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
                ' "$out/cover-index.html"
              '';

          # Fails `nix flake check` when any tracked file is unformatted,
          # turning the formatter into an enforced CI gate.
          formatting = treefmtEval.${system}.config.build.check self;

          # The docs package builds with `mkdocs --strict`, so a broken link or
          # a page missing from the nav fails the build.
          docs = self.packages.${system}.docs;
        }
      );

      apps = forAllSystems (
        system:
        let
          pkgs = nixpkgs.legacyPackages.${system};
          test = pkgs.writeShellApplication {
            name = "cl-regex-kit-test";
            runtimeInputs = [
              pkgs.sbcl
              pkgs.coreutils
            ];
            text = ''
              export CL_SOURCE_REGISTRY="${sourceRegistry}"
              exec timeout 120 sbcl --script ${self}/run-tests.lisp
            '';
          };
          coverage = pkgs.writeShellApplication {
            name = "cl-regex-kit-coverage";
            runtimeInputs = [
              pkgs.sbcl
              pkgs.coreutils
            ];
            text = ''
              export CL_SOURCE_REGISTRY="${sourceRegistry}"
              export CL_REGEX_KIT_ROOT="${self}"
              export CL_REGEX_KIT_COVERAGE_DIRECTORY="$PWD/coverage"
              exec timeout 120 sbcl --script ${./run-coverage.lisp}
            '';
          };
        in
        {
          default = {
            type = "app";
            program = "${test}/bin/cl-regex-kit-test";
            meta.description = "Run the cl-regex-kit test suite";
          };
          test = {
            type = "app";
            program = "${test}/bin/cl-regex-kit-test";
            meta.description = "Run the cl-regex-kit test suite";
          };
          coverage = {
            type = "app";
            program = "${coverage}/bin/cl-regex-kit-coverage";
            meta.description = "Generate SBCL coverage reports";
          };
        }
      );

      devShells = forAllSystems (
        system:
        let
          pkgs = nixpkgs.legacyPackages.${system};
        in
        {
          default = pkgs.mkShell {
            packages = [ pkgs.sbcl ];
            CL_SOURCE_REGISTRY = sourceRegistry;
          };
        }
      );
    };
}
