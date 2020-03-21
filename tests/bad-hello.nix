{ pkgs ? import <nixpkgs> {}, emacs ? import ./emacs.nix }:
let
  check-package = import ../.;
in check-package {
  inherit emacs pkgs;
  pname = "bad-hello";
  version = "0.1";
  src = ./.;
  files = ["tests/bad-hello.el"];
  dependencies = epkgs: [];
  recipe = pkgs.writeText "recipe" ''
(hello :fetcher github :repo "akirak/emacs-package-checker"
       :files ("tests/bad-hello.el"))
'';
}
