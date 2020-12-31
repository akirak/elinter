{
  description = "Linting and testing framework for Emacs Lisp projects";

  inputs.nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
  inputs.flake-utils.url = "github:numtide/flake-utils";
  inputs.flake-compat = {
    url = "github:edolstra/flake-compat";
    flake = false;
  };
  inputs.emacs-overlay.url = "github:nix-community/emacs-overlay";
  inputs.ansi = {
    url = "github:fidian/ansi";
    flake = false;
  };
  inputs.buttercup = {
    url = "github:jorgenschaefer/emacs-buttercup";
    flake = false;
  };
  inputs.gitignore = {
    url = "github:hercules-ci/gitignore";
    flake = false;
  };
  inputs.melpazoid = {
    url = "github:riscy/melpazoid";
    flake = false;
  };
  inputs.nix-elisp-helpers = {
    url = "github:akirak/nix-elisp-helpers";
    flake = false;
  };
  inputs.nix-emacs-ci = {
    url = "github:purcell/nix-emacs-ci";
    flake = false;
  };
  inputs.pre-commit-hooks = {
    url = "github:cachix/pre-commit-hooks.nix";
    flake = false;
  };

  outputs = { nixpkgs, flake-utils, flake-compat, ... }@inputs:
    (flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs {
          inherit system;
        };

        pkgsWithEmacsOverlay = (import nixpkgs {
          inherit system;
        }).extend inputs.emacs-overlay.overlay;

        gitignoreSource = (import inputs.gitignore {
          inherit (pkgs) lib;
        }).gitignoreSource;

        elinterLib = import ./nix/lib.nix;

        defaultLinters = [ "checkdoc" "check-declare" "package-lint " ];

        makeElinterEnv = { linters ? defaultLinters }:
          rec {
            inherit linters;

            lintersAsString = pkgs.lib.concatStringsSep " " linters;

            src = gitignoreSource ./.;

            share = pkgs.runCommandNoCC "elinter-share"
              {
                inherit src;
                # TODO: Remove preferLocalBuild (Maybe it's possible)
                preferLocalBuild = true;
              }
              ''
                mkdir -p $out/share/elinter
                cp -r -t $out/share/elinter $src/share/*.* $src/nix

                substituteInPlace $out/share/elinter/nix/lib.nix \
                --replace './sources.nix' "$out/share/elinter/nix/sources.nix"
              '';
          };

      in
      rec {
        packages.elinter = lib.elinter { };
        packages.elinter-file-linter = lib.elinter { };
        defaultPackage = packages.elinter;

        apps.elinter = flake-utils.mkApp {
          drv = packages.elinter;
        };
        defaultApp = apps.elinter;

        lib = rec {
          elinter-file-linter = options: pkgs.callPackage ./nix/elinter-file-linter {
            inherit elinterLib;
            elinterEnv = makeElinterEnv options;
          };

          elinter-linters = options: pkgs.callPackage ./nix/elinter-linters {
            inherit elinterLib;
            elinterEnv = makeElinterEnv options;
          };

          elinter = options: pkgs.callPackage ./nix/elinter {
            elinter-file-linter = elinter-file-linter options;
            elinter-linters = elinter-linters options;
            elinterEnv = makeElinterEnv options;
          };

          # Export for shell.nix of this project
          gitignoreSource = gitignoreSource;
          pre-commit-hooks = (import inputs.pre-commit-hooks);
        };

      }))
    // { inherit flake-compat; };
}
