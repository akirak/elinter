{ pkgs ? import <nixpkgs> {}
}:
with pkgs;
with (import (import ./nix/sources.nix).gitignore { inherit (pkgs) lib; });
let
  ansi = fetchTarball (import ./nix/sources.nix).ansi.url;
  bashLib = ./lib/workflow.bash;
in
rec {
  main = stdenv.mkDerivation rec {
    name = "elinter";
    version = "0";

    nativeBuildInputs = [ makeWrapper ];

    propagatedBuildInputs = [ bashLib linters ];

    src = gitignoreSource ./.;

    phases = [ "installPhase" ];

    installPhase = ''
      mkdir -p $out

      cp $src/bin/elinter $out
      sed -i "2isource ${bashLib}" $out/elinter

      mkdir -p $out/share/elinter
      lib=$out/share/elinter
      cd $src
      cp -r -t $lib nix copySource.nix fetchSource.nix emacs.nix \
        dynamicVersions.nix

      mkdir -p $out/bin
      makeWrapper $out/elinter $out/bin/elinter \
        --argv0 elinter \
        --prefix PATH : ${linters + "/bin"} \
        --set ELINTER_VERSION $version \
        --set ELINTER_ANSI_LIBRARY ${ansi}/ansi \
        --set ELINTER_NIX_LIB_DIR $lib
    '';
  };

  linters = stdenv.mkDerivation {
    name = "elinter-linters";
    version = "0";

    src = gitignoreSource ./.;

    nativeBuildInputs = [ makeWrapper bashLib ];

    phases = [ "installPhase" ];

    installPhase =
      let

        lint-runner = writeShellScriptBin "elinter-run-linters" ''
          exec emacs -Q --batch --script ${./lisp/elinter-run-linters.el} "$@"
        '';

        colorizer = writeShellScript "elinter-colorizer" ''
           if command -v tput >/dev/null && [[ $(tput colors) -ge 8 ]]; then
             ANSI_SUCCESS=$(tput setaf 2)$(tput bold)
             ANSI_ERROR=$(tput setaf 1)$(tput bold)
             ANSI_CAPTION=$(tput setaf 5)
             ANSI_RESET=$(tput sgr0)
             exec sed -e "s/\\(SUCCESS\\|OK\\)/''${ANSI_SUCCESS}\\1''${ANSI_RESET}/" \
               -e "s/\\(FAIL\\(ED\\)\\?\\)/''${ANSI_ERROR}\\1''${ANSI_RESET}/" \
               -e "s/^\\(Running\\|Compiling\\|Checking\\) .\\+\\.\\{3\\}$/''${ANSI_CAPTION}\\0''${ANSI_RESET}/"
          else
             exec cat
           fi
        '';

      in
        ''
          mkdir -p $out/bin

          # Install the colorizer
          cp ${colorizer} $out/bin/elinter-colorizer

          # Patch backend scripts to redirect output to the colorizer
          for bin in $src/bin-helpers/* ${lint-runner}/bin/*; do
            makeWrapper $bin $out/bin/`basename $bin` \
              --run 'exec &> >(elinter-colorizer)'
          done

          sed -i "2i. ${bashLib}" $out/bin/elinter-byte-compile
        '';
  };

}
