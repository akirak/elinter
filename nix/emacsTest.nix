# nix-shell for elisp package testing
{ emacs
, loadPath
, mainFiles
, caskFile ? null
, extraPackReqs ? []
, extraBuildInputs ? (_: [])
, extraBuildInputsFromNixPkgs ? []
}:
with builtins;
let
  elinterLib = import ./lib.nix;

  unquotedMainFiles = map (pathStr: /. + pathStr) (elinterLib.splitQuotedString mainFiles);

  caskReqs =
    if isString caskFile && caskFile != ""
    then elinterLib.packageDependenciesFromCask (readFile (/. + caskFile))
    else [];

  reqs = lib.flatten (
    map
      (file: elinterLib.packageDependenciesFromMainSource (readFile file))
      unquotedMainFiles
  );

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

in
mkShell {
  buildInputs = [
    (
      elinterLib.emacsDerivation {
        inherit emacs;
        dependencies = lib.unique (reqs ++ caskReqs ++ extraPackReqs);
      }
    )
  ]
  ++ (extraBuildInputs { inherit pkgs; })
  ++ (map derivationFromNixPkgName normalizedNixBuildInputs);

  # Environment variables
  EMACSLOADPATH = concatStringsSep ":" (splitQuotedString loadPath) + ":";
}
