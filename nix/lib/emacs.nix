let pkgs = import ../pkgs.nix;
in with pkgs.lib; {
  emacsVersionToDerivation = emacs-ci: version:
    getAttrFromPath [ ("emacs-" + replaceStrings [ "." ] [ "-" ] version) ]
    emacs-ci;

  emacsVersions = emacs-ci:
    let
      names = builtins.attrNames emacs-ci;
      nameToVersion = name:
        replaceStrings [ "-" ] [ "." ] (removePrefix "emacs-" name);
    in map nameToVersion names;

  emacsWithPackages = emacsDerivation:
    (pkgs.emacsPackagesNgGen emacsDerivation).emacsWithPackages;
}
