{ pkgs ? import <nixpkgs> {},
  emacs ? import ./emacs.nix,
  # srcDir ? ./.,
  testDir ? "",
  packageFile ? ./packages.dhall
}:
import ../. {
  inherit pkgs emacs packageFile;
  srcDir = ../.;
  testDir = ./. + "/${testDir}";
}
