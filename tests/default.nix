{ emacs ? import ./emacs.nix,
  # In typical situation, this should be ./.
  srcDir ? ../.,
  packageFile ? "tests/melpa-packages.dhall"
}:
import ../. {
  inherit emacs packageFile srcDir;
}
