{ pkgs ? import <nixpkgs> {}, emacs ? import ./emacs.nix }:
let
  check-package = import ../.;
in check-package {
  inherit emacs pkgs;
  pname = "bad-hello";
  src = ./.;
  files = ["bad-hello.el"];
  dependencies = epkgs: [];
}
