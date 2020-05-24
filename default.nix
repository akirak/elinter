{ emacs ? "snapshot", srcDir ? null, packageFile ? ".melpa-check/packages.dhall"
, emacs-ci ? (import ./nix/pkgs.nix).emacs-ci }:
with (import ./nix/lib);
with builtins;
let
  pkgs = import ./nix/pkgs.nix;

  # Example:
  # > nix-shell default.nix -A shellWithoutBuild --run 'for p in $packages; do echo $p; packageEmacsVersions $p; done'
  allEmacsVersions = sortEmacsVersions (emacsVersions emacs-ci);

  latestEmacsVersion = with pkgs.lib;
    last (builtins.filter (v: v != "snapshot") allEmacsVersions);

  # The base Emacs derivation used in this file
  emacsDerivation = with pkgs.lib;
    assert (isString emacs || isAttrs emacs);
    # If emacs is a string, assume it is a version
    if isString emacs then
      emacsVersionToDerivation emacs-ci
      (if emacs == "latest" then latestEmacsVersion else emacs)
    else
      emacs;

  # Emacs taking a list of packages as an argument
  emacsWithPackages_ = emacsWithPackages emacsDerivation;

  # Built-in checkers and test drivers
  checkers = import ./nix/checkers { inherit pkgs emacsDerivation; };

  emacsWithLocalPackages = packages_:
    emacsWithPackages_
    (epkgs: (pkgs.lib.forEach packages_ checkers.melpaBuild));

  shellHookWithPackageInfo = packages_:
    with pkgs.lib;
    with builtins;
    let setEmacsVersion = p: "emacsVersion[${p.pname}]=${p.emacsVersion}";
    in ''
      # An indexed array for storing a list of package names
      packages=(${
        builtins.concatStringsSep " " (forEach packages_ (p: p.pname))
      })
      # An associative array for storing the minimum Emacs version for each package
      ${concatMapStringsSep "\n"
      (p: "packageEmacsVersion[${p.pname}]=${p.emacsVersion}") packages_}

      allEmacsVersions=(${builtins.concatStringsSep " " allEmacsVersions})

      # Print a list of available Emacs versions since $1.
      emacsVersionsAfter() {
        local start="$1"
        local count=''${#allEmacsVersions[*]}
        local started=0

        for i in $(seq 1 $count); do
          local ver=''${allEmacsVersions[$i]}
          if [[ $ver = $start ]]; then
            started=1
          fi
          if [[ $started -eq 0 ]]; then
            continue
          fi
          echo $ver
        done
      }

      # Print a list of Emacs versions supported by package $1.
      packageEmacsVersions() {
        local package="$1"
        emacsVersionsAfter ''${packageEmacsVersion[$package]}
      }

      melpaCheckFile() {
        readlink -e .melpa-check-tmp || readlink -e .melpa-check
      }

      melpaCheckNixBuild() {
        nix-build --quiet --no-out-link "$@" `melpaCheckFile`
      }

      melpaCheckNixShell() {
        NIX_BUILD_SHELL=bash nix-shell --pure --quiet "$@" `melpaCheckFile`
      }
    '';

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

  mapPackage1 = f: task: (mapPackage f // { default = f (onlyPackage task); });

  mapPackageWithDefault = f: default: mapPackage f // { default = f default; };

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
    else
      forEachPackage checkers.byte-compile;
  in mapPackage checkers.byte-compile // { inherit default; };

  checkdoc = mapPackageWithDefault checkers.checkdoc (firstPackage // {
    pname = firstPackage.pname + "-all";
    files = concatLists (forEachPackage (p: p.files));
  });

  package-lint = mapPackage1 checkers.package-lint "package-lint";

  preparePackageLint =
    mapPackage1 checkers.preparePackageLint "preparePackageLint";

  # A task to silent build output in buttercup.
  # To be run by nix-build with --no-build-output as a preparation step.
  prepareButtercup = mapPackage1 checkers.prepareButtercup "prepareButtercup";

  buttercup = mapPackage1 checkers.buttercup "buttercup";

  # A task to silent build output in buttercup.
  # To be run by nix-build with --no-build-output as a preparation step.
  prepareErt = mapPackage1 checkers.prepareErt "prepareErt";

  ert = mapPackage1 checkers.ert "ert";

  # A task to silent build output in all tests.
  # To be run by nix-build with --no-build-output as a preparation step.
  prepareAllTests = mapPackage1 checkers.prepareAllTests "prepareAllTests";

  allTests = mapPackage1 checkers.allTests "allTests";

  prepareShell = allOrOne emacsWithLocalPackages;

  shell = let
    mkShellWithEmacsPackages = packages_:
      with pkgs.lib;
      pkgs.mkShell {
        buildInputs = [ (emacsWithLocalPackages packages_) ];
        shellHook = shellHookWithPackageInfo packages_;
      };
  in allOrOne mkShellWithEmacsPackages;

  # Example:
  #
  # > nix-shell default.nix -A shellWithoutBuild --run 'set -e; for p in ${packages[*]}; do nix-build -A byte-compile.$p; done'
  shellWithoutBuild = let
    mkShellWithPackageInfo = packages_:
      with pkgs.lib;
      pkgs.mkShell {
        # Allow running nix-build/nix-shell inside the shell
        buildInputs = [ pkgs.nix ];
        shellHook = shellHookWithPackageInfo packages_;
      };
  in allOrOne mkShellWithPackageInfo;

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
