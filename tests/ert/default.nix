{ emacs ? import ../emacs.nix,
  srcDir ? ../../., packageFile ? "tests/ert/melpa-packages.dhall" }:
import ../../. { inherit emacs packageFile srcDir; }
