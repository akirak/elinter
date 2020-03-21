{ pkgs ? import <nixpkgs> {},
  emacs ? import ./emacs.nix
}:
let
  check-package = import ../.;
in check-package {
  inherit emacs pkgs;
  pname = "hello";
  version = "0.1";
  src = ./.;
  files = ["hello.el"];
  dependencies = epkgs: (with epkgs.melpaStablePackages; [
    dash
  ]);
  recipe = pkgs.writeText "recipe" ''
(hello :fetcher github :repo "akirak/emacs-package-checker"
       :files ("hello.el"))
'';
}
