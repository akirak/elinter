{ pkgs ? import <nixpkgs> {} }:
with (import ./nix/hooks.nix { inherit pkgs; });
pkgs.mkShell {
  shellHook = pre-commit-check.shellHook;
}
