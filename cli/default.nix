{ pkgs ? import <nixpkgs> {} }:
let
  deps = (import ./deps.nix) { inherit pkgs; };
  spago = deps.spago;
  purs = deps.purs;
  spago2nix = deps.spago2nix;

  spagoPkgs = import ./spago-packages.nix { inherit pkgs; };

  base = {
    name ="melpa-check";
    version = "0.1";
    src = ./.;
  };

  dists = {
    cli-with-node = pkgs.stdenv.mkDerivation (base // {

      buildInputs = [purs pkgs.makeWrapper];

      buildPhase = ''
        cd ${spagoPkgs.mkBuildProjectOutput { inherit purs; src = ./.; }}
        ${spago}/bin/spago bundle-app --main Main --no-install --no-build --to $out/index.js
      '';

      installPhase = ''
        mkdir -p $out/bin
        target=$out/bin/melpa-check

        >>$target echo "#!${pkgs.nodejs}/bin/node"
        >>$target echo "require('$out/index.js')";

        chmod +x $target

        wrapProgram $target \
          --prefix PATH : ${pkgs.lib.makeBinPath [
          pkgs.nodejs
        ]}
      '';
    });

    cli-for-github-action = pkgs.stdenv.mkDerivation (base // {

      buildPhase = "";

      installPhase = ''
        mkdir -p $out/bin
        target=$out/bin/melpa-check

        >>$target echo "#!/usr/bin/env node"
        >>$target echo "require('${./dist.js}')";

        chmod +x $target
      '';    
    });   
  };

in
dists.cli-with-node
//
{
  gh-action = dists.cli-for-github-action;
}
