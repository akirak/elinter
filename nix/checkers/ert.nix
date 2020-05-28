config@{ pkgs, customEmacsPackages, ... }:
package:
with (import ../lib);

let
  melpaBuild = import ./melpa-build.nix config;
  patterns = package.ertTests;
  testFiles = discoverFiles package.src patterns;
  makeTestCommand = file: ''
    echo "Running tests in ${file}..."
    emacs --batch --no-site-file \
        --load package --eval '(setq package-archives nil)' \
        -f package-initialize \
        --load ert -l ${file} -f ert-run-tests-batch-and-exit
    r=$?
    e=$((e + r))
    echo ----------------------------------------------------------
  '';
  testCommands = pkgs.lib.concatMapStringsSep "\n" makeTestCommand testFiles;
in makeTestDerivation {
  inherit package testCommands patterns testFiles;
  drvNameSuffix = "-ert";
  title = "ERT Tests";
  typeDesc = "ERT tests";
  emacsWithPackagesDrv =
    (customEmacsPackages.emacsWithPackages (epkgs: [ (melpaBuild package) ]));
}
