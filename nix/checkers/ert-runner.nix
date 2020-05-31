config@{ pkgs, ... }:
package:
with (import ../lib { inherit pkgs; });
with (import ./test-base.nix config);
let
  testLibraries = epkgs: [ epkgs.melpaPackages.ert-runner ];

  emacsWithPackagesDrv = emacsDerivationForTesting package testLibraries;

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
  inherit emacsWithPackagesDrv testCommands testLibraries;
  patterns = [ "test/*.el" ];
  testFiles =
    if builtins.pathExists "${package.src}/test" then [ "test" ] else [ ];
}
