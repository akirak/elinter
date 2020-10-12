{ pkgs ? import <nixpkgs> {}
}:
with pkgs;
with (import (import ./nix/sources.nix).gitignore { inherit (pkgs) lib; });
let
  ansi = fetchTarball (import ./nix/sources.nix).ansi.url;
in
rec {
  # Main derivation: Install elinter executable which is the main
  # entry point of this application.

  # This derivation only contains installPhase, so it probably can
  # be replaced with runCommand.
  main = stdenv.mkDerivation rec {
    name = "elinter";
    version = "0";

    nativeBuildInputs = [ makeWrapper ];

    propagatedBuildInputs = [
      linters
    ];

    src = gitignoreSource ./.;

    phases = [ "installPhase" ];

    installPhase = ''
      mkdir -p $out/bin

      cp $src/bin/elinter $out/bin/elinter

      # Install libraries to share/elinter directory
      mkdir -p $out/share/elinter
      lib=$out/share/elinter
      cd $src
      cp -r -t $lib nix
      cp -t $lib ${ansi}/ansi $src/share/workflow.bash

      # Substitute paths to the library source files.
      substituteInPlace $out/bin/elinter \
        --replace "ansi/ansi" "$lib/ansi" \
        --replace "share/workflow.bash" "$lib/workflow.bash" \
        --replace 'share/nix/' "$lib/nix/"

      mkdir -p $out/bin
      wrapProgram $out/bin/elinter \
        --argv0 elinter \
        --prefix PATH : ${linters + "/bin"} \
        --set ELINTER_VERSION $version
    '';
  };

  # Install linter executables which wrap linting backends.
  # Each wrapper must conform to the standard API of elinter,
  # e.g. pass information via certain environment variables.
  linters = stdenv.mkDerivation {
    name = "elinter-linters";
    version = "0";

    src = gitignoreSource ./.;

    nativeBuildInputs = [ makeWrapper ];

    phases = [ "installPhase" ];

    installPhase =
      let

        # Wrapper for static checkers written in Emacs Lisp.
        lint-runner = writeShellScriptBin "elinter-run-linters" ''
          export ELINTER_LINT_CUSTOM_FILE="''${ELINTER_LINT_CUSTOM_FILE:-${./share/lint-options.el}}"
          exec emacs -Q --batch --script ${./lisp/elinter-run-linters.el} "$@"
        '';

        # A script which post-processes output from wrappers.
        # 
        # It colorizes the output and optionally duplicate the output
        # to a log file.
        logger = writeShellScript "elinter-logger" ''
           if [[ -v ELINTER_LOG_FILE && -n "''${ELINTER_LOG_FILE}" ]]; then
             exec < <(tee -a "''${ELINTER_LOG_FILE}")
           fi

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

        # Helper script for GitHub Actions which adds annotations to
        # indicate error locations.
        github-logger = writeShellScript "elinter-github-logger" ''
          errors=$(mktemp)
          filelist=$(mktemp)

          sed -f "$(dirname $0)/../share/elinter-linters/github-log.sed" "''${ELINTER_LOG_FILE}" \
            | grep -E "^::(error|warning)" > $errors

          grep -oP 'file=\K.+(?=,line=)' $errors \
            | sort | uniq > $filelist

          for f in $(cat $filelist); do
            echo "$f:"
            grep -E "^::(error|warning) file=$f," $errors
          done
        '';

      in
        ''
          mkdir -p $out/bin
          mkdir -p $out/share/elinter-linters

          # Install the logger
          cp ${logger} $out/bin/elinter-logger

          cp $src/share/github-log.sed $out/share/elinter-linters
          cp $src/share/workflow.bash $out/share/elinter-linters
          cp ${github-logger} $out/bin/elinter-github-logger

          # Patch the byte-compile helper to enable GitHub workflow features
          mkdir -p $out/bin-helpers
          cp $src/bin-helpers/* $out/bin-helpers

          substituteInPlace $out/bin-helpers/elinter-byte-compile \
            --replace "share/workflow.bash" "$out/share/elinter-linters/workflow.bash"

          # Patch backend scripts to redirect output to the logger
          for bin in $out/bin-helpers/* ${lint-runner}/bin/*; do
            makeWrapper $bin $out/bin/`basename $bin` \
              --run "exec &> >(elinter-logger)"
          done

        '';
  };

  # Alternative interface which receives files as arguments and run
  # static linting on them.
  #
  # This can be used to implement checks in the Git pre-commit hook.
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
