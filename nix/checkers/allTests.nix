# Run all tests configured in the package
config@{ pkgs, ... }:
package:
with (import ../lib { inherit pkgs; });
with (import ./test-base.nix config);
with builtins;
let
  testDrivers = package.testDrivers;
  ert = import ./ert.nix config package;
  ert-runner = import ./ert-runner.nix config package;
  buttercup = import ./buttercup.nix config package;
in makeTestDerivation {
  inherit package;
  drvNameSuffix = "-test";
  title = "Tests";
  typeDesc = "tests";
  testLibraries = epkgs: concatLists (map (f:
    f {
      ert = ert.testLibraries epkgs;
      ert-runner = ert-runner.testLibraries epkgs;
      buttercup = buttercup.testLibraries epkgs;
    }) testDrivers);
  patterns = concatLists (map (f:
    f {
      ert = ert.patterns;
      ert-runner = ert-runner.patterns;
      buttercup = buttercup.patterns;
    }) testDrivers);
  testFiles = concatLists (map (f:
    f {
      ert = ert.testFiles;
      ert-runner = ert-runner.testFiles;
      buttercup = buttercup.testFiles;
    }) testDrivers);
  testCommands = concatStringsSep "\n" (map (f:
    f {
      ert = ert.testCommands;
      ert-runner = ert-runner.testCommands;
      buttercup = buttercup.testCommands;
    }) testDrivers);
}
