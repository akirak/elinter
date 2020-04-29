let
  sources = import ./sources.nix;
  overlay = _: pkgs: {
    dhall-nix = import sources.easy-dhall-nix {};
    emacs-ci = import sources.nix-emacs-ci {};
  };
  pkgs = import sources.nixpkgs {
    overlays = [ overlay ];
    config = {};
  };
in pkgs
