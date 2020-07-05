config@{ pkgs, customEmacsPackages, ... }:
package:
with (import ../lib { inherit pkgs; });
with (import ./test-base.nix config);
let
  emacsWithPackagesDrv =
    customEmacsPackages.emacsWithPackages (epkgs: [ (melpaBuild package) ]);
  drv = pkgs.stdenv.mkDerivation {
    name = package.pname + "-loadability";
    buildInputs = [ emacsWithPackagesDrv ];
    shellHook = ''
      echo
      echo ==========================================================
      echo Load ${package.pname} package
      echo ==========================================================
      emacs --version
      echo
      emacs --no-site-file --batch -l ${package.pname}
      result=$?
      echo ----------------------------------------------------------
      if [[ $result -eq 0 ]]; then
        echo "Successfully loaded the package."
      else
        echo "Failed to load the package."
      fi
      # Prevent from actually entering the shell
      exit $result
    '';
  };
in drv // { inherit emacsWithPackagesDrv; }
