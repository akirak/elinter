# A basic overlay
# 
# emacs-overlay is not defined here, since it should allow overriding by the user.
let
  sources = import ./sources.nix;
  easy-purescript-nix = import sources.easy-purescript-nix { };
  overlay = _: pkgs: {
    dhall-nix = import sources.easy-dhall-nix { };
    emacs-ci = import sources.nix-emacs-ci;
    spago = easy-purescript-nix.spago;
    purs = easy-purescript-nix.purs;
  };
in overlay
