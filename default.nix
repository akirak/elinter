{ pkgs ? import <nixpkgs> {}
}:
with pkgs;
with (import (import ./nix/sources.nix).gitignore { inherit (pkgs) lib; });
rec {
  main = stdenv.mkDerivation {
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
      cp -r -t $lib nix copySource.nix emacs.nix

      mkdir -p $out/bin
      makeWrapper ${./bin/elinter} $out/bin/elinter \
        --argv0 elinter \
        --prefix PATH : ${linters + "/bin"} \
        --set ELINTER_NIX_LIB_DIR $lib
    '';
  };

  linters = stdenv.mkDerivation {
    name = "elinter-linters";
    version = "0";

    src = gitignoreSource ./.;

    phases = [ "installPhase" ];

    # Since I want to have the executable elisp scripts run by various
    # version of Emacs, I have to prevent from patching shebangs,
    # i.e. keep '!/usr/bin/env emacs'.
    dontPatchShebangs = true;

    installPhase = ''
      mkdir -p $out/bin
      cp $src/bin-helpers/* $out/bin
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
