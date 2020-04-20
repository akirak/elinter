{ pkgs ? import <nixpkgs> {} }:
let
  easyPS = import (pkgs.fetchFromGitHub {
    owner = "justinwoo";
    repo = "easy-purescript-nix";
    rev = "aa3e608608232f4a009b5c132ae763fdabfb4aba";
    sha256 = "0y6jikncxs9l2zgngbd1775f1zy5s1hdc5rhkyzsyaalcl5cajk8";
    # date = 2020-03-16T09:31:55+01:00;
  }) {};
  spago = easyPS.spago;
  purs = easyPS.purs;

in pkgs.stdenv.mkDerivation {

  name = "melpa-check";
  version = "0.1";
  src = ./.;

  buildInputs = [purs pkgs.makeWrapper];

  buildPhase = ''
    ${spago}/bin/spago bundle-app --no-install --no-build --to $out/index.js
  '';

  installPhase = ''
    mkdir -p $out/bin
    target=$out/bin/melpa-check

    >>$target echo "#!/usr/bin/env node"
    >>$target echo "require('$out/index.js')";

    chmod +x $target

    wrapProgram $target \
      --prefix PATH : ${pkgs.lib.makeBinPath [
      pkgs.nodejs
    ]}
'';
}
