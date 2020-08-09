{ pkgs ? import <nixpkgs> {}
}:
with pkgs;
with (import (import ./nix/sources.nix).gitignore { inherit (pkgs) lib; });
rec {
  main = stdenv.mkDerivation rec {
    name = "elinter";
    version = "0";

    nativeBuildInputs = [ makeWrapper ];

    propagatedBuildInputs = [ linters ];

    src = gitignoreSource ./.;

    phases = [ "installPhase" ];

    installPhase = ''
      mkdir -p $out

      mkdir -p $out/share/elinter
      lib=$out/share/elinter
      cd $src
      cp -r -t $lib nix copySource.nix fetchSource.nix emacs.nix \
        dynamicVersions.nix

      mkdir -p $out/bin
      makeWrapper ${./bin/elinter} $out/bin/elinter \
        --argv0 elinter \
        --prefix PATH : ${linters + "/bin"} \
        --set ELINTER_VERSION $version \
        --set ELINTER_NIX_LIB_DIR $lib
    '';
  };

  linters = stdenv.mkDerivation {
    name = "elinter-linters";
    version = "0";

    src = gitignoreSource ./.;

    nativeBuildInputs = [ makeWrapper ];

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
        '';
  };

  # It is possible to use makem.sh for linting, but its interface
  # does not fit this framework (e.g. the way it installs packages),
  # so I won't use it for now.
  # 
  # makem = writeScriptBin "makem.sh"
  #   (readFile ((fetchTarball
  #     (import ./nix/sources.nix."makem.sh".url) + "/makem.sh")))

}
