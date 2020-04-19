{ pkgs ? import <nixpkgs> {},
  emacs ? import ./emacs.nix,
  # In typical situation, this should be ./.
  srcDir ? ../.,
  testDir ? ./.,
  packageFile ? "tests/bad-packages.dhall"
}:
import ../. {
  inherit pkgs emacs packageFile srcDir testDir;
}
