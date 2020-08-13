{ pkgs ? import <nixpkgs> {
    overlays = [
      (import (import ./sources.nix).emacs-overlay)
    ];
  }
, emacs
, loadPath
, mainFiles
}:
with builtins;
with pkgs;
let
  parseQuotedStrings = str:
    let
      m = match "'([^']+)'( +(.+))?" str;
      first = elemAt m 0;
      rest = elemAt m 2;
    in
      if m == null
      then []
      else [ first ] ++ (
        if rest == null
        then []
        else parseQuotedStrings rest
      );

  parseLib = pkgs.callPackage
    ((import ./sources.nix).emacs-overlay + "/parse.nix") {};

  parseReqs = file:
    parseLib.parsePackagesFromPackageRequires (readFile (/. + file));

  reqs = lib.unique (lib.flatten (map parseReqs (parseQuotedStrings mainFiles)));

  package =
    if match "emacs-.+" emacs != null
    then emacs-ci."${emacs}"
    else pkgs."${emacs}";

  emacsWithDependencies =
    (emacsPackagesFor package).emacsWithPackages (
      epkgs:
        map (name: epkgs.${name}) reqs
    );

  loadPathString = concatStringsSep ":" (parseQuotedStrings loadPath) + ":";

in
mkShell {
  buildInputs = [
    emacsWithDependencies
  ];

  shellHook = ''
    export EMACSLOADPATH="${loadPathString}"
  '';
}
