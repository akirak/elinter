{ pkgs ? import <nixpkgs> {},
  system ? builtins.currentSystem,
  emacs ? pkgs.emacs
  # elpaCache ? "/dev/shm/package-lint/elpa/${pname}"
}:
packages:
let
  emacsWithPackages = (pkgs.emacsPackagesNgGen emacs).emacsWithPackages;
  utils = rec {
    checkdoc = package: derivation {
      inherit system;
      inherit (package) src files;
      name = package.pname + "-checkdoc";
      builder = "${pkgs.bash}/bin/bash";
      args = [ ./checkdoc.sh ];
      buildInputs = [ pkgs.coreutils emacs ];
    };

    # Since package-lint requires the internet connection to test
    # if dependencies are installable, you can only run this command
    # in nix-shell, and not in nix-build.
    package-lint = package: pkgs.stdenv.mkDerivation {
      name = package.pname + "-package-lint";
      buildInputs = [
        (emacsWithPackages
          (epkgs:
            (package.dependencies epkgs) ++
            [ epkgs.melpaPackages.package-lint ]))
      ];
      shellHook =
        let
          # Assume the items of files never contain space
          args = pkgs.lib.foldr (a: b: a + " " + b) "" package.files;
          localDeps = pkgs.lib.concatMapStringsSep " " (pkg: pkg.pname)
            (package.localDependencies or []);
          mainFile = package.mainFile or "";
        in ''
    set -e
    cd ${package.src}
    emacs --no-site-file --batch \
       --eval "(setq explicitly-installed-packages '(${localDeps}))" \
       --eval "(setq package-lint-main-file \"${mainFile}\")" \
       -l ${./run-package-lint.el} ${args}
    echo "package-lint is OK."
    # Prevent from actually entering the shell
    exit
    '';
    };

    buttercup = package:
      let
        emacsWithButtercup = emacsWithPackages
          (epkgs:
            [epkgs.melpaPackages.buttercup]
            # ++ package.dependencies epkgs
            ++ [melpaBuild package]);
      in
        pkgs.stdenv.mkDerivation {
          name = package.pname + "-buttercup";
          buildInputs = [ emacsWithButtercup ];
          shellHook =
            ''
      echo "Running buttercup..."
      set -e
      out=$(mktemp)
      cd ${package.src}
      emacs --batch --no-site-file \
          --load package --eval '(setq package-archives nil)' \
          -f package-initialize \
          --load buttercup -f buttercup-run-discover
      exit
      '';
        };

    melpaBuild = package:
      pkgs.emacsPackages.melpaBuild {
        inherit (package) pname version src files recipe;
        packageRequires = package.dependencies pkgs.emacsPackages;
      };
   
  };
  forEachPackage = pkgs.lib.forEach (pkgs.lib.attrValues packages);
in
{
  checkdoc = forEachPackage utils.checkdoc;
  byte-compile = forEachPackage utils.melpaBuild;
  package-lint =
    pkgs.lib.mapAttrs (name: package: utils.package-lint package) packages;
}
