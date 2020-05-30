config@{ pkgs, customEmacsPackages, ... }:
package:
with (import ../lib { inherit pkgs; });

let
  melpaBuild = import ./melpa-build.nix config;
  patterns = package.buttercupTests;
  testFiles = discoverFiles package.src patterns;
in makeTestDerivation2 {
  inherit package patterns testFiles;
  testLibrary = "buttercup";
  batchTestFunction = "buttercup-run";
  drvNameSuffix = "-buttercup";
  title = "Buttercup Tests";
  typeDesc = "buttercup tests";
  emacsWithPackagesDrv = (customEmacsPackages.emacsWithPackages (epkgs:
    [ epkgs.melpaPackages.buttercup (melpaBuild package) ]
    ++ package.testDependencies epkgs));
}
