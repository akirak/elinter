{
  description = "An Emacs Lisp lint runner";

  inputs.flake-utils.url = "github:numtide/flake-utils";
  # inputs.fromElisp = {
  #   url = "github:talyz/fromElisp";
  #   flake = false;
  # };
  inputs.pre-commit-hooks = {
    url = "github:cachix/pre-commit-hooks.nix";
    inputs.nixpkgs.follows = "nixpkgs";
    inputs.flake-utils.follows = "flake-utils";
  };
  inputs.gitignore = {
    url = "github:hercules-ci/gitignore.nix";
    inputs.nixpkgs.follows = "nixpkgs";
  };
  inputs.emacs-overlay = {
    url = "github:nix-community/emacs-overlay";
    inputs.nixpkgs.follows = "nixpkgs";
  };

  outputs = { self, nixpkgs, flake-utils, gitignore, pre-commit-hooks, emacs-overlay }:
    flake-utils.lib.eachSystem
      [
        "x86_64-linux"
        "x86_64-darwin"
        "aarch64-linux"
        "i686-linux"
      ]
      (
        system:
        let
          pkgs = import nixpkgs {
            inherit system;
            overlays = [
              emacs-overlay.overlay
            ];
          };
          inherit (gitignore.lib) gitignoreSource;
          # fromElisp = (import inputs.fromElisp) { inherit pkgs; };
          elispLinter = pkgs.callPackage ./lint.nix {
            src = gitignoreSource ./lisp;
          };
        in
        {
          checks = {
            pre-commit-check = pre-commit-hooks.lib.${system}.run {
              src = gitignoreSource ./.;
              hooks = {
                nixpkgs-fmt.enable = true;
                nix-linter.enable = true;
              };
            };
          };
          devShell = nixpkgs.legacyPackages.${system}.mkShell {
            buildInputs = [
              (elispLinter {
                recipeDir = ./.recipes;
                package = pkgs.emacs;
              })
            ];

            inherit (self.checks.${system}.pre-commit-check) shellHook;
          };
        }
      );
}
