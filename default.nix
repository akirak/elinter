{ pkgs ? import <nixpkgs> {},
  system ? builtins.currentSystem,
  emacs ? pkgs.emacs,
  testDir ? null
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

    melpaBuild = package:
      pkgs.emacsPackages.melpaBuild {
        inherit (package) pname version src files recipe;
        packageRequires = package.dependencies pkgs.emacsPackages;
      };
   
  };
  forEachPackage = pkgs.lib.forEach (pkgs.lib.attrValues packages);
  byte-compile = forEachPackage utils.melpaBuild;
  mapPackage = f: pkgs.lib.mapAttrs (name: package: f package) packages;
in
{
  inherit byte-compile;
  checkdoc = forEachPackage utils.checkdoc;
  package-lint = mapPackage utils.package-lint;
  buttercup = pkgs.stdenv.mkDerivation {
    name = (builtins.head (pkgs.lib.attrValues packages)).pname + "-buttercup";
    buildInputs = [
      (emacsWithPackages
        (epkgs:
          [epkgs.melpaPackages.buttercup]
          ++ byte-compile))
    ];
    shellHook =
      ''
      echo "Running buttercup..."
      set -e
      out=$(mktemp)
      cd ${testDir}
      emacs --batch --no-site-file \
          --load package --eval '(setq package-archives nil)' \
          -f package-initialize \
          --load buttercup -f buttercup-run-discover
      exit
      '';
  };
  shell = mapPackage (package: pkgs.mkShell {
    buildInputs = [
      (emacsWithPackages (epkgs: [(utils.melpaBuild package)]))
    ];
  });
}
