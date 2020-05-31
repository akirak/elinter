config@{ pkgs, ... }:
package:
with (import ../lib { inherit pkgs; });
with (import ./test-base.nix config);
let
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
  testLibraries = _: [];
}
