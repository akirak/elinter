{ pkgs ? import <nixpkgs> {},
  emacs ? import ./emacs.nix
}:
let
  config = {
    inherit pkgs emacs;
  };
  melpaCheck = import ../.;
  packages = import ./packages.nix { inherit pkgs; };
in melpaCheck config packages
