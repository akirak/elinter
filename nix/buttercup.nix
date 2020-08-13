{ pkgs ? import <nixpkgs> {} }:
let
  archive = builtins.fetchTarball (import ./sources.nix).emacs-buttercup.url;
  buttercupDrv = pkgs.srcOnly {
    name = "emacs-buttercup-bin";
    # TODO: Filter source (only pass bin/buttercup)
    src = archive;
  };
in
[ buttercupDrv ]
