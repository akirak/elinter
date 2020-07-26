{ pkgs ? import <nixpkgs> {} }:
with (import ./default.nix { inherit pkgs; });
pkgs.mkShell {
  shellHook = pre-commit-check.shellHook;
}
