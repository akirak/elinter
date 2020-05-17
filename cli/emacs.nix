# This is a Nix shell environment for using Emacs in CI.
let
  pkgs = import <nixpkgs> { };
  sources = import ../nix/sources.nix;
  emacs-ci = import sources.nix-emacs-ci;
in pkgs.mkShell { buildInputs = [ emacs-ci.emacs-26-3 ]; }
