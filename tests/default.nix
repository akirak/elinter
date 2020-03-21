{ pkgs ? import <nixpkgs> {},
  emacs ? import ./emacs.nix
}:
let
  check-package = import ../.;
in check-package {
  inherit emacs pkgs;
  pname = "hello";
  src = ./.;
  files = ["hello.el"];
  dependencies = epkgs: (with epkgs.melpaStablePackages; [
    dash
  ]);
}
