# An attribute set containing derivations for linting
{ sources ? null
, # Main file of the package, given as an absolute path string
  mainFile ? null
, emacs ? "emacs"
  # string or list of strings
, linters
, localPackageNames ? ""
, localPackageRoot ? null
}:
with builtins;
let
  pkgs = import ./pkgsWithEmacsOverlay.nix { inherit sources; };

  lintersAsStrings =
    if builtins.isList linters
    then linters
    else builtins.filter builtins.isString (builtins.split " " linters);

  linterPackages = epkgs: import ./linterPackages.nix {
    inherit sources epkgs;
    inherit (pkgs) lib;
  } lintersAsStrings;

  localPackageNamesAsStrings =
    builtins.filter builtins.isString (builtins.split " " localPackageNames);

  emacs-ci = import (import ./sourceWithFallback.nix sources "nix-emacs-ci");

  package =
    if match "emacs-.+" emacs != null
    then emacs-ci."${emacs}"
    else pkgs."${emacs}";

  parseLib = pkgs.callPackage
    ((import ./sources.nix).emacs-overlay + "/parse.nix") {};

  # List of package names declared in Package-Requires header in the
  # main file
  packageRequires = parseLib.parsePackagesFromPackageRequires
    (readFile (/. + mainFile));

in
rec {
  emacsForCI = (pkgs.emacsPackagesFor package).emacsWithPackages (
    epkgs:
      linterPackages epkgs
      ++ map (name: epkgs.${name})
        (pkgs.lib.subtractLists localPackageNamesAsStrings packageRequires)
  );

  # Shell for linting and testing
  shellForCI = pkgs.mkShell {
    buildInputs = [
      emacsForCI
    ];

    EMACSLOADPATH = pkgs.lib.concatMapStrings
      (name: "${localPackageRoot}/${name}:")
      (
        pkgs.lib.intersectLists localPackageNamesAsStrings packageRequires
      );
  };

  # Used for file linter
  emacsForLint =
    # Using a snapshot version of Emacs can require several
    # different versions of Emacs on a host, which requires more
    # storage space if the developer works on many packages.
    #
    # Lock the version to save space.
    (pkgs.emacsPackagesFor emacs-ci.emacs-27-1).emacsWithPackages (
      epkgs:
        linterPackages epkgs ++ [ epkgs.package-build ]
    );

}
