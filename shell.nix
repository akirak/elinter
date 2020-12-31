{ pkgs ? import <nixpkgs> { } }:
let
  defaultNix = import ./compat.nix;
  system = builtins.currentSystem;
  gitignoreSource = defaultNix.lib.${system}.gitignoreSource;
  elinter-file-linter = defaultNix.packages.${system}.elinter-file-linter;
  pre-commit = defaultNix.lib.${system}.pre-commit-hooks.run {
    src = gitignoreSource ../.;
    excludes = [ "^nix/sources\.nix$" ];
    hooks = {
      shellcheck.enable = true;
      nix-linter.enable = true;
      nixpkgs-fmt.enable = true;

      # TODO: Enable this after finishing the rewrite
      # elinter = {
      #   enable = true;
      #   name = "elinter";
      #   description = "Lint Emacs Lisp files";
      #   entry = "${elinter-file-linter}/bin/elinter-lint-files";
      #   files = "\\.el$";
      # };

    };
  };
in
pkgs.mkShell {
  inherit (pre-commit) shellHook;
}
