{ pkgs ? import <nixpkgs> {}
}:
with pkgs;
with (import (import ./nix/sources.nix).gitignore { inherit (pkgs) lib; });
let
  ansi = fetchTarball (import ./nix/sources.nix).ansi.url;
  bashLib = ./share/workflow.bash;
  runningInsideGitHubActions =
    if builtins.getEnv "GITHUB_ACTIONS" == "true"
    then "true"
    else "false";
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
      cp -r -t $lib nix

      mkdir -p $out/bin
      makeWrapper $out/elinter $out/bin/elinter \
        --argv0 elinter \
        --prefix PATH : ${linters + "/bin"} \
        --set ELINTER_VERSION $version \
        --set ELINTER_ANSI_LIBRARY ${ansi}/ansi \
        --set ELINTER_NIX_LIB_DIR "$lib/nix"
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
          export ELINTER_LINT_CUSTOM_FILE="''${ELINTER_LINT_CUSTOM_FILE:-${./share/lint-options.el}}"
          exec emacs -Q --batch --script ${./lisp/elinter-run-linters.el} "$@"
        '';

        colorizer = writeShellScript "elinter-colorizer" ''
           if command -v tput >/dev/null && [[ $(tput colors) -ge 8 ]]; then
             ANSI_SUCCESS=$(tput setaf 2)$(tput bold)
             ANSI_ERROR=$(tput setaf 1)$(tput bold)
             ANSI_WARN=$(tput setaf 3)$(tput bold)
             ANSI_CAPTION=$(tput setaf 5)
             ANSI_RESET=$(tput sgr0)
             exec sed -e "s/\\(SUCCESS\\|OK\\)/''${ANSI_SUCCESS}\\1''${ANSI_RESET}/" \
               -e "s/\\(WARN\\?\\)/''${ANSI_WARN}\\1''${ANSI_RESET}/" \
               -e "s/\\(FAIL\\(ED\\)\\?\\)/''${ANSI_ERROR}\\1''${ANSI_RESET}/" \
               -e "s/^\\(Running\\|Compiling\\|Checking\\) .\\+\\.\\{3\\}$/''${ANSI_CAPTION}\\0''${ANSI_RESET}/"
          else
             exec cat
           fi
        '';

        github-logger = writeShellScript "elinter-github-logger" ''
          sed -f "$(dirname $0)/../share/elinter/github-log.sed"
        '';

      in
        ''
          mkdir -p $out/bin
          mkdir -p $out/share/elinter

          # Install the colorizer
          cp ${colorizer} $out/bin/elinter-colorizer

          if [[ ${runningInsideGitHubActions} = true ]]; then
            mkdir $out/lib
            cp $src/share/github-log.sed $out/share/elinter
            cp ${github-logger} $out/bin/elinter-github-logger
            extra_substitutors=" | elinter-github-logger"
          else
            extra_substitutors=""
          fi

          # Patch the byte-compile helper to enable GitHub workflow features
          mkdir -p $out/bin-helpers
          cp $src/bin-helpers/* $out/bin-helpers
          sed -i "2i. ${bashLib}" $out/bin-helpers/elinter-byte-compile

          # Patch backend scripts to redirect output to the colorizer
          for bin in $out/bin-helpers/* ${lint-runner}/bin/*; do
            makeWrapper $bin $out/bin/`basename $bin` \
              --run "exec &> >(elinter-colorizer''${extra_substitutors})"
          done

        '';
  };

  file-linter =
    let
      # TODO: Allow overriding this
      pkgsWithOverlay = import <nixpkgs> {
        overlays = [
          (import (import ./nix/sources.nix).emacs-overlay)
        ];
      };
      emacsCi = import (import ./nix/sources.nix).nix-emacs-ci;
      defaultLinters = (import ./nix/emacs.nix { inherit pkgs; }).defaultLinters;
      enabledLinters = defaultLinters ++ [ "melpazoid" ];
      linterPackages = epkgs: import ./nix/linterPackages.nix {
        inherit epkgs lib;
      } enabledLinters;
      emacsForLint =
        (
          # Using a snapshot version of Emacs can require several
          # different versions of Emacs on a host, which requires more
          # storage space if the developer works on many packages.
          #
          # Lock the version to save space.
          (pkgsWithOverlay.emacsPackagesFor emacsCi.emacs-27-1).emacsWithPackages (
            epkgs:
              linterPackages epkgs ++ [ epkgs.package-build ]
          )
        );
    in
      writeShellScriptBin "elinter-lint-files" ''
        export ELINTER_LINTERS="${lib.concatStringsSep " " enabledLinters}"
        export ELINTER_LINT_CUSTOM_FILE="''${ELINTER_LINT_CUSTOM_FILE:-${./share/lint-options.el}}"
        exec ${emacsForLint}/bin/emacs -Q --batch --script \
           ${./lisp/elinter-run-linters.el} "$@"
      '';

}
