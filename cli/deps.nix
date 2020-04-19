{ pkgs ? import <nixpkgs> {} }:
let
  easyPS = import (builtins.fetchTarball
    "https://github.com/justinwoo/easy-purescript-nix/archive/master.tar.gz"
  ) {};
  inputs = {
    inherit (easyPS) spago purs;
  };
  buildInputs = builtins.attrValues inputs;
  shell = pkgs.mkShell {
    buildInputs = easyPS.buildInputs ++ [pkgs.nodejs];
  };
in
inputs // {
  inherit buildInputs shell;
}
