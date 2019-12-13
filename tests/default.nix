{ pkgs ? import <nixpkgs> {},
  emacs ? import ./emacs.nix
}:
let
  check-package = import (builtins.fetchTarball "https://github.com/akirak/emacs-package-checker/archive/v1/master.tar.gz");
in check-package {
  inherit emacs pkgs;
  name = "emacs-package-checker-hello";
  src = ./.;
  targetFiles = ["hello.el"];
  emacsPackages = epkgs: (with epkgs.melpaStablePackages; [
    dash
  ]);
}
