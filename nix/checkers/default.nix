config: {
  checkdoc = import ./checkdoc.nix config;
  package-lint = import ./package-lint.nix config;
  byte-compile = import ./byte-compile.nix config;
  melpaBuild = import ./melpa-build.nix config;
  buttercup = import ./buttercup.nix config;
}
