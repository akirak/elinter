{ pkgs ? import <nixpkgs> {},
  emacs ? import ./emacs.nix,
  testDir ? ./.
}:
let
  config = {
    inherit pkgs emacs testDir;
  };
  melpaCheck = import ../.;
  packages = import ./packages.nix { inherit pkgs; };
in melpaCheck config packages
