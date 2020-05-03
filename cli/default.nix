{ pkgs ? import <nixpkgs> { } }:
with pkgs;
let
  version = "0.1";
  base = {
    inherit version;
    name = "melpa-check";
    src = ./.;
  };

  app = ./dist.js;

  dists = {
    cli-with-node = pkgs.stdenv.mkDerivation (base // {

      buildInputs = [ makeWrapper ];

      buildPhase = "";

      installPhase = ''
        mkdir -p $out/bin
        target=$out/bin/melpa-check

        >>$target echo "#!${pkgs.nodejs}/bin/node"
        >>$target echo "require('${app}')";

        chmod +x $target

        wrapProgram $target \
          --prefix PATH : ${lib.makeBinPath [ nodejs ]}
      '';
    });

    cli-for-github-action = stdenv.mkDerivation (base // {

      buildPhase = "";

      installPhase = ''
        mkdir -p $out/bin
        target=$out/bin/melpa-check

        >>$target echo "#!/usr/bin/env node"
        >>$target echo "require('${app}')";

        chmod +x $target
      '';
    });
  };

in dists.cli-with-node // { gh-action = dists.cli-for-github-action; }
