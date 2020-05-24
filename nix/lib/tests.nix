let pkgs = import ../pkgs.nix;
in with pkgs.lib;
with builtins;
let
  shTrueIf = test:
    if test then
      "${pkgs.coreutils}/bin/true"
    else
      "${pkgs.coreutils}/bin/false";
in {
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
            cd ${package.src}
            echo ==========================================================
            echo ${title} on ${package.pname}
            echo ==========================================================
            ${fileInfo}
            emacs --version
            echo ----------------------------------------------------------
          '';
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
          ${testCommands}
          ${footer}
        '';
      };
    in drv // { inherit emacsWithPackagesDrv testCommands; };
}
