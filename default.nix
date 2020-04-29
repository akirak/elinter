{ emacs, srcDir ? null, packageFile ? ".melpa-check/packages.dhall" }:
with (import ./nix/lib);
with builtins;
let
  pkgs = import ./nix/pkgs.nix;
  # The base Emacs derivation used in this file
  emacsDerivation = with pkgs.lib;
    assert (isString emacs || isAttrs emacs);
    # If emacs is a string, assume it is a version
    if isString emacs then
      emacsVersionToDerivation emacs
    else
      emacs;

  # Emacs taking a list of packages as an argument
  emacsWithPackages_ = emacsWithPackages emacsDerivation;

  # Built-in checkers and test drivers
  checkers = import ./nix/checkers { inherit pkgs emacsDerivation; };

  readDhallPackageList = file:
    parsePackageList srcDir (dhallToNix srcDir file);

  # A collection of local packages as an attr set
  packages =
    with pkgs.lib;
    assert (builtins.isString packageFile);
    let packagePath = srcDir + "/${packageFile}";
    in assert pathExists packagePath;
    if hasSuffix ".dhall" packageFile then
      readDhallPackageList packageFile
      # Nix
    else
      import packagePath { inherit pkgs; };

  # Apply a function on each package
  forEachPackage =
    with pkgs.lib;
    forEach (attrValues packages);

  # Generate an attr set from packages with a function applied on each value
  mapPackage =
    f: with pkgs.lib; mapAttrs (name: package: f package) packages;

in {

  byte-compile = forEachPackage checkers.byte-compile;

  checkdoc = mapPackage checkers.checkdoc // checkers.checkdoc
    (head (attrValues packages) // {
      pname = (head (attrValues packages)).pname + "-all";
      files = concatLists (forEachPackage (p: p.files));
    });

  package-lint = mapPackage checkers.package-lint;

  # A task to silent build output in buttercup.
  # To be run by nix-build with --no-build-output as a preparation step.
  prepareButtercup = forEachPackage (package:
    emacsWithPackages_
    (epkgs: [ epkgs.melpaPackages.buttercup (checkers.melpaBuild package) ]));

  buttercup = mapPackage checkers.buttercup;

  shell = let
    mkShellWithEmacsPackages = packages:
      mkShell {
        buildInputs =
          [ (emacsWithPackages_ (epkgs: (forEachPackage checkers.melpaBuild))) ];
      };
    individuals = mapPackage (package: mkShellWithEmacsPackages [ package ]);
    all = mkShellWithEmacsPackages packages;
    onlyAll = { inherit all; };
  in all // individuals // onlyAll;

  cli = import ./cli;

}
