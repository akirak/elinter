# Run all tests configured in the package
config@{ pkgs, ... }: package:
with (import ../lib { inherit pkgs; });
with builtins;
let
  testDrivers = package.testDrivers;
  ert = import ./ert.nix config package;
  buttercup = import ./buttercup.nix config package;
  hasButtercup = any (f:
    f {
      ert = false;
      buttercup = true;
    }) testDrivers;
in makeTestDerivation {
  inherit package;
  drvNameSuffix = "-test";
  title = "Tests";
  typeDesc = "tests";
  emacsWithPackagesDrv = if hasButtercup then
    buttercup.emacsWithPackagesDrv
  else
    ert.emacsWithPackagesDrv;
  patterns = concatLists (map (f:
    f {
      ert = ert.patterns;
      buttercup = buttercup.patterns;
    }) testDrivers);
  testFiles = concatLists (map (f:
    f {
      ert = ert.testFiles;
      buttercup = buttercup.testFiles;
    }) testDrivers);
  testCommands = concatStringsSep "\n" (map (f:
    f {
      ert = ert.testCommands;
      buttercup = buttercup.testCommands;
    }) testDrivers);
}
