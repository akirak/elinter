config: {
  checkdoc = import ./checkdoc.nix config;
  package-lint = import ./package-lint.nix config;
  preparePackageLint = package:
    (import ./package-lint.nix config package).emacsWithPackagesDrv;
  byte-compile = import ./byte-compile.nix config;
  melpaBuild = import ./melpa-build.nix config;
  buttercup = import ./buttercup.nix config;
  prepareButtercup = package:
    (import ./buttercup.nix config package).emacsWithPackagesDrv;
  load = import ./load.nix config;
  prepareLoad = package:
    (import ./load.nix config package).emacsWithPackagesDrv;
  ert = import ./ert.nix config;
  prepareErt = package: (import ./ert.nix config package).emacsWithPackagesDrv;
  ert-runner = import ./ert-runner.nix config;
  prepareErtRunner = package: (import ./ert-runner.nix config package).emacsWithPackagesDrv;
  allTests = import ./allTests.nix config;
  prepareAllTests = package:
    (import ./allTests.nix config package).emacsWithPackagesDrv;
}
