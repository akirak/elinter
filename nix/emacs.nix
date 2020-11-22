# An attribute set containing derivations for linting
{
  # Main file of the package, given as an absolute path string
  mainFile ? null
, emacs ? "emacs"
  # string or list of strings
, linters
, localPackageNames ? ""
, localPackageRoot ? null
}:
with builtins;
let
  elinterLib = import ./lib.nix;

  pkgsForLib = import <nixpkgs> {};

  lintersAsStrings =
    if builtins.isList linters
    then linters
    else builtins.filter builtins.isString (builtins.split " " linters);

  builtinPackages = [ "checkdoc" "check-declare" ];

  # Some linters are shipped with Emacs.
  # Transform to a list of package names.
  linterPackages =
    filter (name: ! (elem name builtinPackages))
      lintersAsStrings;

  localPackageNamesAsStrings =
    builtins.filter builtins.isString (builtins.split " " localPackageNames);

in
rec {
  emacsForCI = elinterLib.emacsDerivation {
    inherit emacs;
    dependencies =
      linterPackages
      ++ pkgsForLib.lib.subtractLists
        localPackageNamesAsStrings
        (elinterLib.packageDependenciesFromMainSource (readFile (/. + mainFile)));
  };

  # Shell for linting and testing
  shellForCI = pkgsForLib.mkShell {
    buildInputs = [
      emacsForCI
    ];

    EMACSLOADPATH = pkgsForLib.lib.concatMapStrings
      (name: "${localPackageRoot}/${name}:")
      (
        pkgsForLib.lib.intersectLists localPackageNamesAsStrings packageRequires
      );
  };

  # Used for file linter
  emacsForLint =
    elinterLib.emacsDerivation {
      # Using a snapshot version of Emacs can require several
      # different versions of Emacs on a host, which requires more
      # storage space if the developer works on many packages.
      #
      # Lock the version to save space.
      emacs = "emacs-27-1";
      dependencies = [ "package-build" ] ++ linterPackages;
    };

}
