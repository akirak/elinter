let
  sources = import ./sources.nix;
  overlay = _: pkgs: {
    dhall-nix = import sources.easy-dhall-nix {};
    emacs-ci = import sources.nix-emacs-ci;
    spago = (import sources.easy-purescript-nix {}).spago;
    purs = (import sources.easy-purescript-nix {}).purs;
  };
  pkgs = import sources.nixpkgs {
    overlays = [ overlay ];
    config = {};
  };
in pkgs
