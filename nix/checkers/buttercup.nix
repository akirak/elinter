config@{ pkgs, emacsDerivation }:
package:
with (import ../lib);

let
  melpaBuild = import ./melpa-build.nix config;
  patterns = package.buttercupTests;
  testFiles = discoverFiles package.src patterns;
  makeLoadArguments = pkgs.lib.concatMapStringsSep " " (x: "-l " + x);
  shTrueIf = test:
    if test then
      "${pkgs.coreutils}/bin/true"
    else
      "${pkgs.coreutils}/bin/false";
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
  emacsWithPackagesDrv = (emacsWithPackages emacsDerivation
    (epkgs: [ epkgs.melpaPackages.buttercup (melpaBuild package) ]));
  testsDrv = pkgs.stdenv.mkDerivation {
    name = package.pname + "-buttercup";
    buildInputs = [ emacsWithPackagesDrv ];
    shellHook = ''
      e=0
      cd ${package.src}
      echo ==========================================================
      echo Buttercup tests on ${package.pname}
      echo ==========================================================
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
in testsDrv // {
  inherit emacsWithPackagesDrv;
}
