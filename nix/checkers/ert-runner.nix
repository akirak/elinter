config@{ pkgs, customEmacsPackages, ... }:
package:
with (import ../lib { inherit pkgs; });

let
  melpaBuild = import ./melpa-build.nix config;

  emacsWithPackagesDrv = (customEmacsPackages.emacsWithPackages (epkgs:
    [ (melpaBuild package) epkgs.melpaPackages.ert-runner ]
    ++ package.testDependencies epkgs));

  testCommands = withMutableSourceDirectory package ''
    echo "Running ert-runner..."
    emacs --batch --no-site-file \
        --load package --eval '(setq package-archives nil)' \
        -f package-initialize -l ert-runner
  '';

  drv = pkgs.stdenv.mkDerivation {
    name = package.pname + "-ert-runner";
    buildInputs = [ emacsWithPackagesDrv ];
    shellHook = let
      header = makeTestHeader {
        inherit package;
        fileInfo = "";
        title = "ERT-Runner";
      };
    in ''
      ${header}
      e=0
      ${testCommands}
      r=$?
      e=$((e + r))
    '';
  };
in drv // {
  inherit emacsWithPackagesDrv testCommands;
  # Unlike other test drivers, you don't have to expose testFiles and
  # patterns, because they are only used in allTests.nix
}
