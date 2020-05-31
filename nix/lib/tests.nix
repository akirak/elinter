{ pkgs }:
with pkgs.lib;
with builtins;
let
  shTrueIf = test:
    if test then
      "${pkgs.coreutils}/bin/true"
    else
      "${pkgs.coreutils}/bin/false";

  makeTestDerivation = { package, title, typeDesc, patterns, testFiles
    , testCommands, emacsWithPackagesDrv, drvNameSuffix }:
    let
      drv = pkgs.stdenv.mkDerivation {
        name = package.pname + drvNameSuffix;
        buildInputs = [ emacsWithPackagesDrv ];
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
          header = ''
            e=0
            echo ==========================================================
            echo ${title} on ${package.pname}
            echo ==========================================================
            ${fileInfo}
            emacs --version
            echo ----------------------------------------------------------
          '';
          # Enter the immutable directory containing the source files.
          enterImmutableDirectory = ''
            cd ${package.src}
          '';
          # Create a temporary copy of the source directory for testing
          enterMutableDirectory = ''
            set -e
            cd ${package.src}
            tmpdir=$(mktemp -d -u -t ${package.pname}-ertXXX)
            cleanup_tmpdir() { cd ${package.src}; rm -rf $tmpdir; }
            trap cleanup_tmpdir EXIT INT KILL
            cp -r ${package.src} $tmpdir
            chmod u+w -R $tmpdir
            cd $tmpdir
            set +e
          '';
          # TODO: Add an option to switch between mutable and immutable test directory
          enterDirectory = enterMutableDirectory;
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
          ${enterDirectory}
          ${testCommands}
          ${footer}
        '';
      };
    in drv // { inherit emacsWithPackagesDrv testCommands patterns testFiles; };

  makeTestDerivation2 = { package, title, typeDesc, patterns, testFiles
    , testLibrary, batchTestFunction, emacsWithPackagesDrv, drvNameSuffix }:
    let
      makeTestCommand = file: ''
        echo "Running tests in ${file}..."
        cd $root/${builtins.dirOf file}
        emacs --batch --no-site-file \
            --load package --eval '(setq package-archives nil)' \
            -f package-initialize \
            --load ${testLibrary} -l ${baseNameOf file} -f ${batchTestFunction}
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
        emacsWithPackagesDrv drvNameSuffix;
    };

in { inherit makeTestDerivation makeTestDerivation2; }
