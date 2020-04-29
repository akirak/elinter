let
  pkgs = import ../pkgs.nix;
  emacs-ci = pkgs.emacs-ci;
in
with pkgs.lib;
{
  emacsVersionToDerivation = version:
    getAttrsFromPath [ ("emacs-" + replaceStrings "." "-" version) ] emacs-ci;
  emacsWithPackages = emacsDerivation:
    (pkgs.emacsPackagesNgGen emacsDerivation).emacsWithPackages;
}
