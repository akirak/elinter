{ pkgs ? import <nixpkgs> {}, emacs }:
let
  check-package = import ../.;
in check-package {
  inherit emacs pkgs;
  name = "emacs-package-checker-hello";
  src = ./.;
  targetFiles = ["hello.el"];
  emacsPackages = epkgs: (with epkgs.melpaStablePackages; [
    dash
  ]);
}
