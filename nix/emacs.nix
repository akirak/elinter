{ sources ? null
, # Main file of the package, given as an absolute path string
  mainFile ? null
, emacs ? "emacs"
  # String
, enabledLinters ? null
  # List of string
, linters ? if builtins.isList enabledLinters
  then enabledLinters
  else builtins.filter builtins.isString (builtins.split " " enabledLinters)
}:
with builtins;
let
  pkgs = import ./pkgsWithEmacsOverlay.nix { inherit sources; };

  linterPackages = epkgs: import ./linterPackages.nix {
    inherit sources epkgs;
    inherit (pkgs) lib;
  } linters;

  emacs-ci = import (import ./sourceWithFallback.nix sources "nix-emacs-ci");

  package =
    if match "emacs-.+" emacs != null
    then emacs-ci."${emacs}"
    else pkgs."${emacs}";

in
rec {
  emacsForCI = pkgs.emacsWithPackagesFromPackageRequires {
    inherit package;
    packageElisp = readFile (/. + mainFile);
    extraEmacsPackages = linterPackages;
  };

  # Shell for linting and testin
  shellForCI = pkgs.mkShell {
    buildInputs = [
      emacsForCI
    ];
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
