{ pkgs ? import <nixpkgs> {}
}:
with pkgs;
with (import (import ./sources.nix).gitignore { inherit (pkgs) lib; });
let
  pre-commit-hooks = import (import ./sources.nix)."pre-commit-hooks.nix";
in
{

  pre-commit-check = pre-commit-hooks.run {
    src = gitignoreSource ../.;
    excludes = [ "^nix/sources\.nix$" ];
    hooks = {
      shellcheck.enable = true;
      nix-linter.enable = true;
      nixpkgs-fmt.enable = true;
    };
  };

}
