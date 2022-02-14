{ lib
, writeShellApplication
, emacsCIVersions
}:
# Arguments specific to a repository
{ minimumEmacsVersion
, emacsConfig
}:
with builtins;
let
  emacsVersions = lib.pipe (attrNames emacsCIVersions) [
    (filter (name:
      name == "emacs-snapshot"
      || compareVersions
        (replaceStrings [ "-" ] [ "." ] (lib.removePrefix "emacs-" name))
        minimumEmacsVersion >= 0))
  ];

  makeScriptDerivation = { name, emacs, text }: writeShellApplication {
    inherit name;
    runtimeInputs = [
      emacs
    ];
    inherit text;
  };
in
prefix:
{ text
, compile ? false
, matrix ? true }:
let
  origDerivation = makeScriptDerivation {
    name = prefix;
    emacs = emacsConfig.override { inherit compile; };
    inherit text;
  };

  makeDerivationForEmacsVersion = emacsVersion: makeScriptDerivation {
    name = "${prefix}-${emacsVersion}";
    emacs = emacsConfig.override {
      emacs = emacsCIVersions.${emacsVersion};
      inherit compile;
    };
    inherit text;
  };
in
if matrix
then lib.extendDerivation true
  {
    matrix = lib.genAttrs emacsVersions makeDerivationForEmacsVersion;
  }
  origDerivation
else origDerivation

