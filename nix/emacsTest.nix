{ pkgs ? import <nixpkgs> {
    overlays = [
      (import (import ./sources.nix).emacs-overlay)
    ];
  }
, emacs
, loadPath
, mainFiles
, caskFile ? null
, extraPackReqs ? []
, extraBuildInputs ? (_: [])
, extraBuildInputsFromNixPkgs ? []
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

  helpers = (import (import ./sources.nix).nix-elisp-helpers { inherit pkgs; });

  caskPackage = helpers.parseCask (readFile (/. + caskFile));

  caskReqs =
    if isString caskFile && caskFile != ""
    then map head (
      (caskPackage.dependencies or []) ++ ((caskPackage.development or {}).dependencies or [])
    )
    else [];

  reqs = lib.flatten (map parseReqs (parseQuotedStrings mainFiles));

  emacs-ci = import (import ./sources.nix).nix-emacs-ci;

  package =
    if match "emacs-.+" emacs != null
    then emacs-ci."${emacs}"
    else pkgs."${emacs}";

  emacsWithDependencies =
    (emacsPackagesFor package).emacsWithPackages (
      epkgs:
        map (name: epkgs.${name}) (lib.unique (reqs ++ caskReqs ++ extraPackReqs))
    );

  loadPathString = concatStringsSep ":" (parseQuotedStrings loadPath) + ":";

  normalizedNixBuildInputs =
    if isString extraBuildInputsFromNixPkgs && extraBuildInputsFromNixPkgs != ""
    then filter isString (split " " extraBuildInputsFromNixPkgs)
    else if isList extraBuildInputsFromNixPkgs
    then extraBuildInputsFromNixPkgs
    else throw "extraBuildInputsFromNixPkgs must be either a string or a list";

  derivationFromNixPkgName = name:
    lib.attrByPath (filter isString (split "\\." name))
      (throw ("Cannot find a derivation in the nixpkgs: " + name))
      pkgs;

  otherBuildInputs =
    (extraBuildInputs { inherit pkgs; })
    ++ map derivationFromNixPkgName normalizedNixBuildInputs;

in
mkShell {
  buildInputs = [
    emacsWithDependencies
  ] ++ otherBuildInputs;

  shellHook = ''
    export EMACSLOADPATH="${loadPathString}"
  '';
}
