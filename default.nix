{ pkgs ? import <nixpkgs> {},
  system ? builtins.currentSystem,
  name ? "awesome-emacs-package",
  emacs ? pkgs.emacs,
  emacsPackages, src, targetFiles
}:
let
  # Emacs with packages specified as dependencies from outside of this
  # nix file. This is used for byte-compiling the package.
  emacsWithPackages = (pkgs.emacsPackagesNgGen emacs).emacsWithPackages
    emacsPackages;
  # Emacs with package-lint. This is used for running package-lint.
  emacsForPackageLint = (pkgs.emacsPackagesNgGen emacs).emacsWithPackages
    (epkgs: (with epkgs.melpaStablePackages; [ package-lint ]));
  # At present, elpa packages are stored in shared memory, per package.
  #
  # Package archives remain on memory unless you delete them manually,
  # and the (nearly) same package database is cached for every package,
  # which take some disk space.
  #
  # I don't think this can be a serious issue
  elpaCache = "/dev/shm/package-lint/elpa/${name}";
in rec
{

  byte-compile = derivation {
    inherit src targetFiles system;
    name = name + "-byte-compile";
    builder = "${pkgs.bash}/bin/bash";
    args = [ ./byte-compile.sh ];
    buildInputs = with pkgs; [ coreutils emacsWithPackages ];
  };

  checkdoc = derivation {
    inherit src targetFiles system;
    name = name + "-checkdoc";
    builder = "${pkgs.bash}/bin/bash";
    args = [ ./checkdoc.sh ];
    buildInputs = with pkgs; [ coreutils emacs ];
  };

  # Since package-lint requires the internet connection to test
  # if dependencies are installable, you can only run this command
  # in nix-shell, and not in nix-build.
  package-lint = pkgs.stdenv.mkDerivation {
    name = name + "-package-lint";
    buildInputs = [ emacsForPackageLint ];
    shellHook =
    let
    # Assume the items of targetFiles never contain space
      args = pkgs.lib.foldr (a: b: a + " " + b) "" targetFiles;
    in ''
    set -e
    cd ${src}
    ELPA_USER_DIR=${elpaCache} emacs --batch -l ${./run-package-lint.el} ${args}
    echo "package-lint is OK."
    # Prevent from actually entering the shell
    exit
    '';
  };

}
