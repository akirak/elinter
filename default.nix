{ pkgs ? import <nixpkgs> {},
  system ? builtins.currentSystem,
  emacs ? pkgs.emacs,
  srcDir ? null,
  testDir ? null,
  packageFile
  # elpaCache ? "/dev/shm/package-lint/elpa/${pname}"
}:
with pkgs.lib;
let
  emacsWithPackages = (pkgs.emacsPackagesNgGen emacs).emacsWithPackages;
  utils = rec {
    checkdoc = package:
      assert (builtins.isPath package.src);
      assert (builtins.pathExists package.src);
      assert (builtins.all (file:
        let
          srcPath = package.src + "/${file}";
        in
          builtins.trace srcPath (builtins.pathExists srcPath)
      ) package.files);
      derivation {
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
          mainFile =
            # package.mainFile can be null if the package is converted
            # from Dhall, so the null check is necessary.
            if package ? mainFile && !(isNull package.mainFile)
            then package.mainFile
            else "";
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
  dhallUtils = rec {
    # Since dhall-nix in nixpkgs is often broken, I will use the
    # binary provided by the source repo.
    # See https://github.com/dhall-lang/dhall-haskell/issues/1624
    easyDhall = import (pkgs.fetchFromGitHub {
      owner = "justinwoo";
      repo = "easy-dhall-nix";
      rev = "35bca5ba56b7b3f8684aa0afbb65608159beb5ce";
      # date = 2020-04-04T12:53:43+02:00;
      sha256 = "16l71qzzfkv4sbxl03r291nswsrkr3g13viqkma2s8r5vy9la3al";
    }) {};
    dhallToNix = file:
      let
        drv = pkgs.stdenv.mkDerivation {
          name = "generate-nix-from-dhall";

          buildCommand = ''
          cd ${srcDir}
          cd ${builtins.dirOf file}
          dhall-to-nix < "${builtins.baseNameOf file}" > $out
          '';

          buildInputs = [ easyDhall.dhall-nix-simple ];
        };
      in import "${drv}";
    parsePackageList = plainPackageList:
      with pkgs.lib;
      let
        localPackages = map (p: p.pname) plainPackageList;
        localMelpaBuild = epkgs: pkg: epkgs.melpaBuild {
          inherit (pkg) pname version src files recipe;
          packageRequires = pkg.dependencies epkgs;
        };
        f = self: builtins.listToAttrs (forEach plainPackageList (x:
          {
            name = x.pname;
            value = x // {
              src = srcDir;
              recipe = pkgs.writeText "recipe" x.recipe;
              dependencies = epkgs: forEach x.dependencies (depName:
                if builtins.elem depName localPackages
                then localMelpaBuild epkgs self."${depName}"
                else epkgs.melpaPackages."${depName}"
              );
              localDependencies = forEach x.localDependencies (depName:
                self."${depName}"
              );
            };
          }));
      in fix f;
    readDhallPackageList = file: parsePackageList (dhallToNix file);
  };
  packages =
    assert (builtins.isString packageFile);
    let packagePath = srcDir + "/${packageFile}";
    in assert (builtins.trace packagePath (builtins.pathExists packagePath));
      if hasSuffix ".dhall" packageFile
      then dhallUtils.readDhallPackageList packageFile
        # Nix
      else import packagePath { inherit pkgs; };
  forEachPackage = pkgs.lib.forEach (pkgs.lib.attrValues packages);
  byte-compile = forEachPackage utils.melpaBuild;
  mapPackage = f: pkgs.lib.mapAttrs (name: package: f package) packages;
in
{
  # Export dhallUtils for testing purposes
  inherit byte-compile dhallUtils;
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
