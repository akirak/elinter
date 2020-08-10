{ pkgs ? import <nixpkgs> {
    overlays = [
      (import (import ./nix/sources.nix).emacs-overlay)
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
  defaultLinters = [ "package-lint" ];

  linters =
    if enabledLinters == null || enabledLinters == ""
    then defaultLinters
    else if isList enabledLinters
    then enabledLinters
    else filter isString (split " " enabledLinters);

  melpazoidSource = (import ./nix/sources.nix).melpazoid;

  linterPackages = epkgs:
    (
      (
        lib.optional (elem "melpazoid" linters) (
          epkgs.trivialBuild {
            pname = "melpazoid";
            version = "0";
            src = fetchTarball melpazoidSource.url;
            postUnpack = "mv $sourceRoot/melpazoid/melpazoid.el $sourceRoot";
            meta = {
              inherit (melpazoidSource) description homepage;
              license = lib.licenses.gpl3;
            };
          }
        )
      )
      ++ map (name: epkgs."${name}") (filter (name: elem name [ "package-lint" ]) linters)
    );

  emacs-ci = import (import ./nix/sources.nix).nix-emacs-ci;

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
}
