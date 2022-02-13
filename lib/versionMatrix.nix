{ lib
, emacsCIVersions
, packageInputs
, localPackages
}:
with builtins;
let
  maxVersion = versions: head (sort (a: b: compareVersions a b > 0) versions);

  minimumEmacsVersion = lib.pipe localPackages [
    (map (ename: packageInputs.${ename}.packageRequires.emacs or null))
    (filter isString)
    maxVersion
  ];
in
lib.pipe (attrNames emacsCIVersions) [
  (filter (name:
    name == "emacs-snapshot"
    || compareVersions
      (replaceStrings [ "-" ] [ "." ] (lib.removePrefix "emacs-" name))
      minimumEmacsVersion >= 0))
]
