config@{ pkgs, emacsDerivation }:
package:
with (import ../lib);

let
  melpaBuild = import ./melpa-build.nix config;
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
      (emacsWithPackages emacsDerivation
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
      if ${if builtins.length testFiles == 0 then "true" else "false"}
      then
        echo "No test files found."
        exit 0
      fi
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
in testsDrv
