{
  # The default version of Emacs to use
  emacs ? "snapshot",
  # You can use niv to update melpa-check
  melpa-check ? import ../../../.,
  # The directory containing source files
  srcDir ? ../../../.,
  # A configuration file which defines packages under test
  packageFile ? "nix/checkers/tests/packages.dhall"
}:
melpa-check {
  inherit emacs packageFile srcDir;
}
