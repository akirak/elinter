# nix-shell for elisp package testing
{ emacs
, loadPath
, mainFiles
, caskFile ? null
, extraPackReqs ? []
  # Used for installing executables at runtime, e.g. buttercup
, extraBuildInputs ? (_: [])
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
  ++ (extraBuildInputs { inherit pkgs; });

  # Environment variables
  EMACSLOADPATH = concatStringsSep ":" (splitQuotedString loadPath) + ":";
}
