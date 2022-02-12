{
  description = "Minimal flake project";

  inputs = {
    flake-utils = {
      url = "github:numtide/flake-utils";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    gitignore = {
      url = "github:hercules-ci/gitignore.nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    twist.url = "github:emacs-twist/twist.nix";
    emacs-ci = {
      url = "github:purcell/nix-emacs-ci";
      flake = false;
    };
    package-lint = {
      url = "github:purcell/package-lint";
      flake = false;
    };

    melpa = {
      url = "github:melpa/melpa";
      flake = false;
    };
    gnu-elpa = {
      url = "git+https://git.savannah.gnu.org/git/emacs/elpa.git?ref=main";
      flake = false;
    };
  };

  outputs =
    { self
    , nixpkgs
    , flake-utils
    , ...
    } @ inputs:
    {
      lib = import ./lib { inherit inputs; };
    } //
    flake-utils.lib.eachSystem [
      # TODO: Use the same set of systems as nix-emacs-ci
      "x86_64-linux"
    ]
      (system:
      let
        inherit (nixpkgs) lib;

        pkgs = import nixpkgs {
          inherit system;
          overlays = [
            (import (inputs.emacs-ci + "/overlay.nix"))
            inputs.twist.overlay
          ];
        };

        elinter = pkgs.callPackage ./pkgs/elinter {
          emacs = pkgs.callPackage ./lib/emacsSmall.nix { };
          inherit inputs;
        };
      in
      {
        packages = flake-utils.lib.flattenTree {
          inherit elinter;
        };
      }
      );
}
