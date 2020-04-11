{ pkgs ? import <nixpkgs> {},
  emacs ? import ./emacs.nix,
  # In typical situation, this should be ./.
  srcDir ? ../.,
  testDir ? ./.,
  packageFile ? "tests/melpa-packages.dhall"
}:
import ../. {
  inherit pkgs emacs packageFile srcDir testDir;
}
