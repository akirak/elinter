{ pkgs ? import <nixpkgs> {
    overlays = [
      (import (import ./nix/sources.nix).emacs-overlay)
    ];
  }
, # Main file of the package, given as an absolute path string
  mainFile ? null
, emacs ? "emacs"
}:
with builtins;
with pkgs;
let
  emacsForCI = emacsWithPackagesFromPackageRequires {
    package = pkgs."${emacs}";
    packageElisp = readFile (/. + mainFile);
    extraEmacsPackages = epkgs: with epkgs; [
      package-lint
    ];
  };

in
{
  # Shell for linting and testin
  development = pkgs.mkShell {
    buildInputs = [
      emacsForCI
    ];
  };
}
