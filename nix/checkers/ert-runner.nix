config@{ pkgs, customEmacsPackages, ... }:
package:
with (import ../lib { inherit pkgs; });
with (import ./test-base.nix config);
let
  # testLibraries = epkgs: [ epkgs.melpaPackages.ert-runner ];

  testLibraries = [ "ert-runner" ];

  # emacsWithPackagesDrv = emacsDerivationForTesting package testLibraries;

  packageNames = builtins.concatStringsSep " " (package.dependencyNames
    ++ package.testDependencyNames ++ [ "ert-runner" ]);

  testCommands = withMutableSourceDirectory package ''
    echo "Running ert-runner..."
    emacs --batch --no-site-file \
        -l ${./setup-package.el} \
        -l ert-runner
    r=$?
    e=$((e + r))
  '';

  drv = pkgs.stdenv.mkDerivation {
    name = package.pname + "-ert-runner";
    buildInputs = [ customEmacsPackages.emacs ];
    shellHook = let
      header = makeTestHeader {
        inherit package;
        fileInfo = "";
        title = "ERT-Runner";
      };
    in ''
      ${header}
      ${packageInstallCommandForTesting package testLibraries}
      e=0
      ${testCommands}
      exit $e
    '';
  };
in drv // {
  inherit emacsWithPackagesDrv testCommands testLibraries;
  patterns = [ "test/*.el" ];
  testFiles =
    if builtins.pathExists "${package.src}/test" then [ "test" ] else [ ];
}
