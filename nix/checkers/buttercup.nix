config@{ pkgs, ... }:
package:
with (import ../lib { inherit pkgs; });
with (import ./test-base.nix config);
let
  patterns = package.buttercupTests;
  testFiles = discoverFiles package.src patterns;
in makeTestDerivation2 {
  inherit package patterns testFiles;
  testLibrary = "buttercup";
  batchTestFunction = "buttercup-run";
  drvNameSuffix = "-buttercup";
  title = "Buttercup Tests";
  typeDesc = "buttercup tests";
  testLibraries = epkgs: [ epkgs.melpaPackages.buttercup ];
}
