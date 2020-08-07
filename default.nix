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

    phases = [ "installPhase" ];

    installPhase =
      let
        lint-runner = writeTextFile {
          name = "elinter-run-linters";
          executable = true;
          text = ''
            exec emacs -Q --batch --script ${./lisp/elinter-run-linters.el} "$@"
          '';
        };
      in
        ''
          mkdir -p $out/bin
          cp $src/bin-helpers/* $out/bin
          cp ${lint-runner} $out/bin/elinter-run-linters
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
