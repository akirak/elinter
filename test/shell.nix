{ pkgs ? import <nixpkgs> {} }:
with pkgs;
mkShell {
  buildInputs = [
    (import ../default.nix { inherit pkgs; }).main
  ];

  shellHook = ''
    ${import ./bad-1.nix {}}
  
    exit
  '';
}
