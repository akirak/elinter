{ emacs ? "snapshot"
, srcDir ? null
, packageFile ? ".melpa-check/packages.dhall"
, emacs-ci ? import (import ./nix/sources.nix).nix-emacs-ci,
  emacs-overlay ? import (builtins.fetchTarball {
    url = "https://github.com/nix-community/emacs-overlay/archive/master.tar.gz";
  })
}:
let
  sources = import ./nix/sources.nix;
  overlay = import ./nix/overlay.nix;
  pkgs = import sources.nixpkgs {
    overlays = [ overlay emacs-overlay ];
    config = { };
  };
in import ./programs.nix {
  inherit emacs srcDir packageFile;
  inherit pkgs emacs-ci;
}
