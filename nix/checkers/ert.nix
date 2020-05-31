config@{ pkgs, customEmacsPackages, ... }:
package:
with (import ../lib { inherit pkgs; });

let
  melpaBuild = import ./melpa-build.nix config;
  patterns = package.ertTests;
  testFiles =
    discoverFilesWithExcludes package.src patterns package.testExcludes;

  ertTestDerivation = makeTestDerivation2 {
    inherit package patterns testFiles;
    testLibrary = "ert";
    batchTestFunction = "ert-run-tests-batch-and-exit";
    drvNameSuffix = "-ert";
    title = "ERT Tests";
    typeDesc = "ERT tests";
    emacsWithPackagesDrv = (customEmacsPackages.emacsWithPackages
      (epkgs: [ (melpaBuild package) ] ++ package.testDependencies epkgs));
  };

  ertRunnerTestDerivation = makeTestDerivation {
    inherit package patterns testFiles;
    testCommands = ''
      emacs --batch --no-site-file \
          --load package --eval '(setq package-archives nil)' \
          -f package-initialize -l ert-runner
          '';
    drvNameSuffix = "-ert-runner";
    title = "ERT Runner Tests";
    typeDesc = "ERT runner tests";
    emacsWithPackagesDrv = (customEmacsPackages.emacsWithPackages (epkgs:
      [ (melpaBuild package) epkgs.melpaPackages.ert-runner ]
      ++ package.testDependencies epkgs));
  };

in if package.ertRunnerCompatible then
  ertRunnerTestDerivation
else
  ertTestDerivation
