config@{ pkgs, customEmacsPackages, ... }:
package:
with (import ../lib { inherit pkgs; });

let
  melpaBuild = import ./melpa-build.nix config;
  patterns = package.ertTests;
  testFiles =
    discoverFilesWithExcludes package.src patterns package.testExcludes;
in makeTestDerivation2 {
  inherit package patterns testFiles;
  testLibrary = "ert";
  batchTestFunction = "ert-run-tests-batch-and-exit";
  drvNameSuffix = "-ert";
  title = "ERT Tests";
  typeDesc = "ERT tests";
  emacsWithPackagesDrv = (customEmacsPackages.emacsWithPackages
    (epkgs: [ (melpaBuild package) ] ++ package.testDependencies epkgs));
}
