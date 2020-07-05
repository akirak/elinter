config@{ pkgs, customEmacsPackages, ... }:
package:
with (import ../lib { inherit pkgs; });
with (import ./test-base.nix config);
let
  emacsWithPackagesDrv = customEmacsPackages.emacsWithPackages (_: [ ]);

  melpazoidEl = "${(import ../sources.nix).melpazoid}/melpazoid/melpazoid.el";

  drv = pkgs.stdenv.mkDerivation {
    name = package.pname + "-melpazoid-misc";
    buildInputs = [ emacsWithPackagesDrv ];
    shellHook = ''
      echo
      echo ==========================================================
      echo melpazoid on ${package.pname} package
      echo ==========================================================
      cd ${package.src}
      emacs --no-site-file --batch \
         --eval "(setq noninteractive nil)" \
         -l ${melpazoidEl} \
         -l ${./melpazoid-misc-runner.el} \
         -f melpazoid-misc-runner-batch \
         ${concatShArgs package.files}
      result=$?
      echo ----------------------------------------------------------
      if [[ $result -eq 0 ]]; then
        echo "No melpazoid errors found."
      else
        echo "Errors found by melpazoid."
      fi
      # Prevent from actually entering the shell
      exit $result
    '';
  };
in drv // { inherit emacsWithPackagesDrv; }

