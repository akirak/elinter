{ pkgs ? import <nixpkgs> {
    overlays = [
      (import (import ./sources.nix).emacs-overlay)
    ];
  }
, # Main file of the package, given as an absolute path string
  mainFile ? null
, emacs ? "emacs"
, enabledLinters ? null
}:
with builtins;
with pkgs;
let
  # What would be the best way to set the default?
  defaultLinters = [ "checkdoc" "package-lint" "check-declare" ];

  linters =
    if enabledLinters == null || enabledLinters == ""
    then defaultLinters
    else if isList enabledLinters
    then enabledLinters
    else filter isString (split " " enabledLinters);

  linterPackages = epkgs: import ./linterPackages.nix { inherit epkgs lib; } linters;

  emacs-ci = import (import ./sources.nix).nix-emacs-ci;

  package =
    if match "emacs-.+" emacs != null
    then emacs-ci."${emacs}"
    else pkgs."${emacs}";

in
rec {
  emacsForCI = emacsWithPackagesFromPackageRequires {
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

  # Export the list for use in file-linter in ../default.nix
  inherit defaultLinters;
}
