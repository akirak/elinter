let
  pkgs = import ../pkgs.nix;

  compareEmacsVersions = v1: v2:
    if v1 == "snapshot" then
      false
    else if v2 == "snapshot" then
      true
    else
      builtins.compareVersions v1 v2 < 0;

in with pkgs.lib; rec {
  emacsVersionToDerivation = emacs-ci: version:
    getAttrFromPath [ ("emacs-" + replaceStrings [ "." ] [ "-" ] version) ]
    emacs-ci;

  emacsVersions = emacs-ci:
    let
      names = builtins.attrNames emacs-ci;
      nameToVersion = name:
        replaceStrings [ "-" ] [ "." ] (removePrefix "emacs-" name);
    in map nameToVersion names;

  inherit compareEmacsVersions;

  sortEmacsVersions = sort compareEmacsVersions;
}
