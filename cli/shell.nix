let
  sources = import ../nix/sources.nix;
  overlay = import ../nix/overlay.nix;
  pkgs = import <nixpkgs> {
    overlays = [ overlay ];
    config = { };
  };
in with pkgs; mkShell { buildInputs = [ spago nodejs purs gnumake ]; }
