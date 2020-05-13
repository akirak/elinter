{ emacs ? "snapshot", srcDir ? null, packageFile ? ".melpa-check/packages.dhall"
, emacs-ci ? (import ./nix/pkgs.nix).emacs-ci }:
with (import ./nix/lib);
with builtins;
let
  pkgs = import ./nix/pkgs.nix;
  # The base Emacs derivation used in this file
  emacsDerivation = with pkgs.lib;
    assert (isString emacs || isAttrs emacs);
    # If emacs is a string, assume it is a version
    if isString emacs then emacsVersionToDerivation emacs-ci emacs else emacs;

  # Emacs taking a list of packages as an argument
  emacsWithPackages_ = emacsWithPackages emacsDerivation;

  # Built-in checkers and test drivers
  checkers = import ./nix/checkers { inherit pkgs emacsDerivation; };

  prepareButtercup = package:
    emacsWithPackages_
    (epkgs: [ epkgs.melpaPackages.buttercup (checkers.melpaBuild package) ]);

  emacsWithLocalPackages = packages_:
    emacsWithPackages_
    (epkgs: (pkgs.lib.forEach packages_ checkers.melpaBuild));

  readDhallPackageList = file: parsePackageList srcDir (dhallToNix srcDir file);

  isDhallProject = pkgs.lib.hasSuffix ".dhall" packageFile;

  # A collection of local packages as an attr set
  packages = with pkgs.lib;
    assert (builtins.isString packageFile);
    let packagePath = srcDir + "/${packageFile}";
    in assert pathExists packagePath;
    if isDhallProject then
      readDhallPackageList packageFile
      # Nix
    else
      import packagePath { inherit pkgs; };

  firstPackage = head (attrValues packages);

  onlyPackageWithMsg = msg:
    let packageValues = attrValues packages;
    in if length packageValues == 1 then head packageValues else abort msg;

  onlyPackage = task:
    onlyPackageWithMsg
    "When you run ${task} and there are multiple packages, you have to specify the name of a package.";

  # Apply a function on each package
  forEachPackage = with pkgs.lib; forEach (attrValues packages);

  # Generate an attr set from packages with a function applied on each value
  mapPackage = f: with pkgs.lib; mapAttrs (name: package: f package) packages;

  # Generate an attr set for both individual packages and all packages.
  #
  # Each package name points to a derivation on a package, and "all"
  # points to a derivation on all packages.
  #
  # Also defaults to all.
  #
  # f should be a function which generates a (shell) derivation from a
  # list of package values.
  allOrOne = f:
    let
      individuals = mapPackage (package: f [ package ]);
      all = f (builtins.attrValues packages);
      onlyAll = { inherit all; };
    in individuals // onlyAll // all;

  verifyJsonPackageList = jsonFile:
    pkgs.stdenv.mkDerivation {
      name = firstPackage.pname + "-meta-check";
      buildInputs = [ emacsDerivation ];
      shellHook = ''
        set -e
        cd ${srcDir}
        emacs --no-site-file --batch -l ${./melpa-check-verify.el} \
        -f melpa-check-verify-package-json-batch ${jsonFile}
        exit $?
      '';
    };

in {

  byte-compile = let
    packageValues = attrValues packages;
    default = if length packageValues == 1 then
      checkers.byte-compile (head packageValues)
    else {
      all = checkers.byte-compile;
    };
  in mapPackage checkers.byte-compile // default;

  checkdoc = mapPackage checkers.checkdoc // checkers.checkdoc (firstPackage
    // {
      pname = firstPackage.pname + "-all";
      files = concatLists (forEachPackage (p: p.files));
    });

  package-lint = mapPackage checkers.package-lint
    // checkers.package-lint (onlyPackage "package-lint");

  # A task to silent build output in buttercup.
  # To be run by nix-build with --no-build-output as a preparation step.
  prepareButtercup = mapPackage prepareButtercup
    # Since this command is likely to be called just before 'buttercup',
    # it can be compatible with it.
    // prepareButtercup (onlyPackage "prepareButtercup");

  buttercup = mapPackage checkers.buttercup
    // checkers.buttercup (onlyPackage "buttercup");

  prepareShell = allOrOne emacsWithLocalPackages;

  shell = let
    mkShellWithEmacsPackages = packages_:
      with pkgs.lib;
      pkgs.mkShell {
        buildInputs = [ (emacsWithLocalPackages packages_) ];
        shellHook =
          let setEmacsVersion = p: "emacsVersion[${p.pname}]=${p.emacsVersion}";
          in ''
            # An indexed array for storing a list of package names
            packages=(${
              builtins.concatStringsSep " " (forEach packages_ (p: p.pname))
            })
            # An associative array for storing the minimum Emacs version for each package
            ${concatMapStringsSep "\n"
            (p: "packageEmacsVersion[${p.pname}]=${p.emacsVersion}") packages_}
          '';
      };
  in allOrOne mkShellWithEmacsPackages;

  meta = assert (builtins.isAttrs packages);
    assert (builtins.length (builtins.attrValues packages) > 0);
    if isDhallProject then
      verifyJsonPackageList (dhallToJson srcDir packageFile)
    else
      builtins.abort "Not supporting non-Dhall projects";

  cli = import ./cli;

  # An exposed API for displaying a list of Emacs versions supported
  # by emacs-ci.
  #
  # You can use this expression as follows:
  #
  # > nix-instantiate --eval -A emacsVersions --strict
  emacsVersions = emacsVersions emacs-ci;

  # An exposed API for displaying a list of package names.
  #
  # > nix-instantiate --eval -A packageNames --strict
  packageNames = forEachPackage (p: p.pname);

}
