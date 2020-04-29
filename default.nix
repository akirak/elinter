{ pkgs ? import <nixpkgs> { }, system ? builtins.currentSystem
, emacs ? pkgs.emacs, srcDir ? null, testDir ? null
, packageFile ? ".melpa-check/packages.dhall" }:
with pkgs.lib;
let
  emacsWithPackages = (pkgs.emacsPackagesNgGen emacs).emacsWithPackages;
  utils = rec {
    concatShArgs = files: pkgs.lib.foldr (a: b: a + " " + b) "" files;
    checkdoc = package:
      assert (builtins.isPath package.src);
      assert (builtins.pathExists package.src);
      assert (builtins.all (file:
        let srcPath = package.src + "/${file}";
        in builtins.pathExists srcPath) package.files);
      pkgs.stdenv.mkDerivation {
        name = package.pname + "-checkdoc";
        buildInputs = [ emacs pkgs.coreutils ];
        shellHook = ''
          echo
          echo ==========================================================
          echo Checkdoc on ${package.pname} package
          echo ==========================================================
          cd ${package.src}
          emacs --batch --no-site-file \
                --load ${./run-checkdoc.el} \
                ${concatShArgs package.files}
          exit $?
        '';
      };

    # Since package-lint requires the internet connection to test
    # if dependencies are installable, you can only run this command
    # in nix-shell, and not in nix-build.
    package-lint = package:
      pkgs.stdenv.mkDerivation {
        name = package.pname + "-package-lint";
        buildInputs = [
          (emacsWithPackages (epkgs:
            (package.dependencies epkgs)
            ++ [ epkgs.melpaPackages.package-lint ]))
        ];
        shellHook = let
          # Assume the items of files never contain space
          localDeps = pkgs.lib.concatMapStringsSep " " (pkg: pkg.pname)
            (package.localDependencies or [ ]);
          mainFile =
            # package.mainFile can be null if the package is converted
            # from Dhall, so the null check is necessary.
            if package ? mainFile && !(isNull package.mainFile) then
              package.mainFile
            else
              "";
        in ''
          echo
          echo ==========================================================
          echo package-lint on ${package.pname} package
          echo ==========================================================
          cd ${package.src}
          emacs --no-site-file --batch \
             --eval "(setq explicitly-installed-packages '(${localDeps}))" \
             --eval "(setq package-lint-main-file \"${mainFile}\")" \
             -l ${./run-package-lint.el} ${concatShArgs package.files}
          result=$?
          echo ----------------------------------------------------------
          if [[ $result -eq 0 ]]; then
            echo "No package-lint errors found."
          else
            echo "Errors found by package-lint."
          fi
          # Prevent from actually entering the shell
          exit $result
        '';
      };

    melpaBuild = package:
      pkgs.emacsPackages.melpaBuild {
        inherit (package) pname version src files recipe;
        packageRequires = package.dependencies pkgs.emacsPackages;
      };

    byte-compile = package:
      derivation {
        inherit system;
        src = srcDir;
        name = package.pname + "-byte-compile";
        builder = "${pkgs.bash}/bin/bash";
        buildInputs =
          [ pkgs.coreutils (emacsWithPackages package.dependencies) ];
        args = [ ./byte-compile.sh ];
        # Only used in the shell script
        files = concatShArgs package.files;
        inherit (package) pname;
        dependencyNames =
          concatShArgs (package.dependencyNames or [ "unknown" ]);
        # localDependencyNames = concatShArgs package.localDependencyNames;
        loadPaths = let
          dirs = pkgs.lib.unique (map builtins.dirOf package.files);
          dquote = file: ''"'' + file + ''"'';
        in "'(${concatShArgs (map dquote dirs)})";
      };

    discoverFiles = rootDir: patterns:
      let
        drv = pkgs.stdenv.mkDerivation {
          name = "bath-glob";
          buildInputs = [ pkgs.bash ];
          buildCommand = ''
            shopt -s extglob nullglob
            cd ${rootDir}
            echo ${concatShArgs patterns} > $out
          '';
        };
        raw = pkgs.lib.fileContents drv;
      in filter (str: builtins.pathExists (rootDir + "/${str}"))
      (pkgs.lib.splitString " " raw);

    buttercup = package:
      let
        patterns = package.buttercupTests;
        testFiles = discoverFiles package.src patterns;
        noTestsDrv = pkgs.stdenv.mkDerivation {
          name = package.pname + "-no-buttercup";
          buildInputs = [ ];
          shellHook = ''
            echo "${package.pname} has no tests."
            exit
          '';
        };
        makeLoadArguments = pkgs.lib.concatMapStringsSep " " (x: "-l " + x);
        makeTestCommand = file: ''
          echo "Running tests in ${file}..."
          emacs --batch --no-site-file \
              --load package --eval '(setq package-archives nil)' \
              -f package-initialize \
              --load buttercup -l ${file} -f buttercup-run
          r=$?
          e=$((e + r))
          echo ----------------------------------------------------------
        '';
        testsDrv = pkgs.stdenv.mkDerivation {
          name = package.pname + "-buttercup";
          buildInputs = [
            (emacsWithPackages
              (epkgs: [ epkgs.melpaPackages.buttercup (melpaBuild package) ]))
          ];
          shellHook = ''
            e=0
            cd ${package.src}
            echo ==========================================================
            echo Buttercup tests on ${package.pname}
            echo ==========================================================
            echo "File patterns: ${builtins.concatStringsSep " " patterns}"
            echo Matched files: ${builtins.concatStringsSep " " testFiles}
            echo
            emacs --version
            echo ----------------------------------------------------------
            ${pkgs.lib.concatMapStringsSep "\n" makeTestCommand testFiles}
            if [[ $e -gt 0 ]]; then
              echo "Some buttercup tests for ${package.pname} have failed."
              exit 1
            else
              echo "All buttercup tests for ${package.pname} have passed."
              exit 0
            fi
          '';
        };
      in testsDrv;
    # if builtins.length nonEmptyTests == 0
    # then noTestsDrv
    # else testsDrv;

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
    }) { };
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
        localMelpaBuild = epkgs: pkg:
          epkgs.melpaBuild {
            inherit (pkg) pname version src files recipe;
            packageRequires = pkg.dependencies epkgs;
          };
        f = self:
          builtins.listToAttrs (forEach plainPackageList (x: {
            name = x.pname;
            value = x // {
              src = srcDir;
              recipe = pkgs.writeText "recipe" x.recipe;
              dependencies = epkgs:
                forEach x.dependencies (depName:
                  if builtins.elem depName localPackages then
                    localMelpaBuild epkgs self."${depName}"
                  else
                    epkgs.melpaPackages."${depName}");
              localDependencies =
                forEach x.localDependencies (depName: self."${depName}");
              # Only used for information to the user
              dependencyNames = x.dependencies;
            };
          }));
      in fix f;
    readDhallPackageList = file: parsePackageList (dhallToNix file);
  };
  packages = assert (builtins.isString packageFile);
    let packagePath = srcDir + "/${packageFile}";
    in assert builtins.pathExists packagePath;
    if hasSuffix ".dhall" packageFile then
      dhallUtils.readDhallPackageList packageFile
      # Nix
    else
      import packagePath { inherit pkgs; };
  forEachPackage = pkgs.lib.forEach (pkgs.lib.attrValues packages);
  mapPackage = f: pkgs.lib.mapAttrs (name: package: f package) packages;
  tasks = rec {
    byte-compile = forEachPackage utils.byte-compile;

    checkdoc = mapPackage utils.checkdoc // utils.checkdoc
      (builtins.head (pkgs.lib.attrValues packages) // {
        pname = (builtins.head (pkgs.lib.attrValues packages)).pname + "-all";
        files = builtins.concatLists (forEachPackage (p: p.files));
      });

    package-lint = mapPackage utils.package-lint;

    # A task to silent build output in buttercup.
    # To be run by nix-build with --no-build-output as a preparation step.
    # onlyBuild = forEachPackage utils.melpaBuild;
    prepareButtercup = forEachPackage (package:
      emacsWithPackages
      (epkgs: [ epkgs.melpaPackages.buttercup (utils.melpaBuild package) ]));

    buttercup = mapPackage utils.buttercup;

    shell = let
      individuals = mapPackage (package:
        pkgs.mkShell {
          buildInputs =
            [ (emacsWithPackages (epkgs: [ (utils.melpaBuild package) ])) ];
        });
      all = pkgs.mkShell {
        buildInputs = [ (emacsWithPackages (epkgs: byte-compile)) ];
      };
      onlyAll = { inherit all; };
    in all // individuals // onlyAll;
  };
in {
  inherit (tasks)
    byte-compile checkdoc package-lint prepareButtercup buttercup shell;
  # Export dhallUtils for testing purposes
  inherit dhallUtils;

  cli = import ./cli;
}
