{ pkgs ? import <nixpkgs> { } }:
with (import ./nix/hooks.nix { inherit pkgs; });
pkgs.mkShell {
  buildInputs = with pkgs; [
    nix-linter
    nixpkgs-fmt
    shellcheck
  ];

  shellHook = pre-commit-check.shellHook;
}
