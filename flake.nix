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
    elsa = {
      url = "github:emacs-elsa/Elsa";
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
    let
      overlay = import ./pkgs/overlay.nix { inherit inputs; };

      inherit (flake-utils.lib) mkApp;
    in
    {
      inherit overlay;
      lib = import ./lib { inherit inputs overlay; };
      templates = {
        simple = {
          path = ./templates/simple;
          description = "A boilerplate for an Emacs Lisp-only project";
        };
        action = {
          path = ./templates/action;
          description = "A GitHub Actions workflow for linting your package";
        };
      };
      defaultTemplate = self.templates.simple;
    } //
    flake-utils.lib.eachDefaultSystem (system:
    let
      inherit (nixpkgs) lib;

      pkgs = import nixpkgs {
        inherit system;
        overlays = [
          overlay
        ];
      };

      admin = pkgs.elinter.emacsConfigForLint.admin "./pkgs/emacs-config/lock";
    in
    {
      packages = flake-utils.lib.flattenTree {
        inherit (pkgs.elinter) elinter elsa;
      };

      apps.lock = mkApp {
        drv = admin.lock;
      };
    }
    );
}
