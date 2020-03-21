{ pkgs ? import <nixpkgs> {},
  system ? builtins.currentSystem,
  pname ? "awesome-emacs-package",
  version,
  recipe,
  emacs ? pkgs.emacs,
  dependencies, src, files,
  elpaCache ? "/dev/shm/package-lint/elpa/${pname}"
}:
let
  # Emacs with packages specified as dependencies from outside of this
  # nix file. This is used for byte-compiling the package.
  emacsWithPackages = (pkgs.emacsPackagesNgGen emacs).emacsWithPackages
    dependencies;
  # Emacs with package-lint. This is used for running package-lint.
  emacsForPackageLint = (pkgs.emacsPackagesNgGen emacs).emacsWithPackages
    (epkgs: (with epkgs.melpaPackages; [ package-lint ]));
in rec
{

  byte-compile = derivation {
    inherit src files system;
    name = pname + "-byte-compile";
    builder = "${pkgs.bash}/bin/bash";
    args = [ ./byte-compile.sh ];
    buildInputs = [ pkgs.coreutils emacsWithPackages ];
  };

  checkdoc = derivation {
    inherit src files system;
    name = pname + "-checkdoc";
    builder = "${pkgs.bash}/bin/bash";
    args = [ ./checkdoc.sh ];
    buildInputs = [ pkgs.coreutils emacs ];
  };

  # Since package-lint requires the internet connection to test
  # if dependencies are installable, you can only run this command
  # in nix-shell, and not in nix-build.
  package-lint = pkgs.stdenv.mkDerivation {
    name = pname + "-package-lint";
    buildInputs = [ emacsForPackageLint ];
    shellHook =
    let
    # Assume the items of files never contain space
      args = pkgs.lib.foldr (a: b: a + " " + b) "" files;
    in ''
    set -e
    cd ${src}
    ELPA_USER_DIR=${elpaCache} emacs --no-site-file --batch -l ${./run-package-lint.el} ${args}
    echo "package-lint is OK."
    # Prevent from actually entering the shell
    exit
    '';
  };

  buttercup =
    let
      emacsWithButtercup = (pkgs.emacsPackagesNgGen emacs).emacsWithPackages
        (epkgs:
          [epkgs.melpaPackages.buttercup]
          # ++ dependencies epkgs
          ++ [melpaBuild]);
    in
      pkgs.stdenv.mkDerivation {
        name = pname + "-buttercup";
        buildInputs = [ emacsWithButtercup ];
        shellHook =
      ''
      echo "Running buttercup..."
      set -e
      out=$(mktemp)
      cd ${src}
      emacs --batch --no-site-file \
          --load package --eval '(setq package-archives nil)' \
          -f package-initialize \
          --load buttercup -f buttercup-run-discover
      exit
      '';
      };

  melpaBuild =
    pkgs.emacsPackages.melpaBuild {
      inherit pname version src files recipe;
      packageRequires = dependencies pkgs.emacsPackages;
    };

}
