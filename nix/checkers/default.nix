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
}
