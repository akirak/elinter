config@{ pkgs, customEmacsPackages, ... }:
package:
with (import ../lib { inherit pkgs; });

let
  melpaBuild = import ./melpa-build.nix config;
  patterns = package.buttercupTests;
  testFiles = discoverFiles package.src patterns;
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
  testCommands = pkgs.lib.concatMapStringsSep "\n" makeTestCommand testFiles;
in makeTestDerivation {
  inherit package testCommands patterns testFiles;
  drvNameSuffix = "-buttercup";
  title = "Buttercup Tests";
  typeDesc = "buttercup tests";
  emacsWithPackagesDrv = (customEmacsPackages.emacsWithPackages
    (epkgs: [ epkgs.melpaPackages.buttercup (melpaBuild package) ]));
}
