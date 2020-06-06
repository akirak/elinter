# Libraries dedicated to testing
config@{ pkgs, customEmacsPackages, ... }:
with pkgs.lib;
with builtins;
with (import ../lib { inherit pkgs; });
let
  shTrueIf = test:
    if test then
      "${pkgs.coreutils}/bin/true"
    else
      "${pkgs.coreutils}/bin/false";

  # Create a temporary copy of the source directory for testing
  # and run commands in the directory
  withMutableSourceDirectory = package: commands: ''
    set -e
    cd ${package.src}
    tmpdir=$(mktemp -d -u -t ${package.pname}-ertXXX)
    cleanup_tmpdir() { cd ${package.src}; rm -rf $tmpdir; }
    trap cleanup_tmpdir EXIT INT KILL
    cp -r ${package.src} $tmpdir
    chmod u+w -R $tmpdir
    cd $tmpdir
    set +e
    ${commands}
  '';

  makeTestHeader = { title, package, fileInfo }: ''
    e=0
    echo ==========================================================
    echo ${title} on ${package.pname}
    echo ==========================================================
    ${fileInfo}
    emacs --version
    echo ----------------------------------------------------------
  '';

  melpaBuild = import ./melpa-build.nix { inherit pkgs customEmacsPackages; };

  packageInstallCommand = packages: ''
    install_log=$(mktemp -t package-installXXX.log)
    cleanup_install_log() { rm ''${install_log}; }
    trap cleanup_install_log EXIT INT KILL TERM
    echo "Installing packages..."
    emacs --batch --no-site-file \
        -l ${./setup-package.el} \
        --eval "(setup-package-many '(${
          builtins.concatStringsSep " " packages
        }))" 2>''${install_log} || {
      cat ''${install_log}
      exit 1
    }
  '';

  packageInstallCommandForTesting = package: testLibraries:
    packageInstallCommand ((pkgs.lib.subtractLists package.localDependencyNames
      package.dependencyNames) ++ package.testDependencyNames ++ testLibraries);

  # emacsDerivationForTesting = package: testLibraries:
  #   customEmacsPackages.emacsWithPackages (epkgs:
  #     [ (melpaBuild package) ] ++ package.testDependencies epkgs
  #     ++ testLibraries epkgs);

  makeTestDerivation = { package, title, typeDesc, patterns, testFiles
    , testCommands, testLibraries, drvNameSuffix }:
    let
      # emacsWithPackagesDrv = emacsDerivationForTesting package testLibraries;
      # Change the directory for testing
      testCommands_ = withMutableSourceDirectory package testCommands;
      drv = pkgs.stdenv.mkDerivation {
        name = package.pname + drvNameSuffix;
        buildInputs = [ customEmacsPackages.emacs ];
        shellHook = let
          fileInfo = ''
            if ${shTrueIf (builtins.length patterns == 0)}; then
              echo "${package.pname} has no test file patterns."
              exit 0
            fi
            echo "File patterns: ${builtins.concatStringsSep " " patterns}"
            echo Matched files: ${builtins.concatStringsSep " " testFiles}
            echo
            if ${shTrueIf (builtins.length testFiles == 0)}; then
              echo "${package.pname} has no matching test files."
              exit 0
            fi
          '';
          header = makeTestHeader { inherit title package fileInfo; };
          #  ''
          #   cd ${package.src}
          #   ${testCommands}
          # '';
          footer = ''
            if [[ $e -gt 0 ]]; then
              echo "Some ${title} for ${package.pname} have failed."
              exit 1
            else
              echo "All ${title} for ${package.pname} have passed."
              exit 0
            fi
          '';
        in ''
          ${header}
          ${packageInstallCommandForTesting package testLibraries}
          ${testCommands_}
          ${footer}
        '';
      };
    in drv // {
      inherit patterns testFiles testLibraries;
      # inherit emacsWithPackagesDrv;
      # Pass the possibly sandboxed test environment
      testCommands = testCommands_;
    };

  makeTestDerivation2 = { package, title, typeDesc, patterns, testFiles
    , testLibrary, batchTestFunction, drvNameSuffix, testLibraries }:
    let
      addLoadPaths =
        let dirs = (pkgs.lib.unique (map builtins.dirOf package.files));
        in concatMapStringsSep " " (dir:
          ''
            --eval "(add-to-list 'load-path (expand-file-name \"$root/${dir}\"))"'')
        dirs;

      makeTestCommand = file: ''
        echo "Running tests in ${file}..."
        cd $root/${builtins.dirOf file}
        emacs --batch --no-site-file \
            -l ${./setup-package.el} \
            ${addLoadPaths} \
            --load ${testLibrary} -l ${baseNameOf file} \
            -f ${batchTestFunction}
        r=$?
        e=$((e + r))
        echo ----------------------------------------------------------
      '';
      testCommands = ''
        root=$PWD
        ${pkgs.lib.concatMapStringsSep "\n" makeTestCommand testFiles}
        cd $root
      '';
    in makeTestDerivation {
      inherit testCommands package title typeDesc patterns testFiles
        testLibraries drvNameSuffix;
    };

in {
  inherit makeTestDerivation makeTestDerivation2 makeTestHeader
    packageInstallCommand packageInstallCommandForTesting
    withMutableSourceDirectory emacsDerivationForTesting;
}
