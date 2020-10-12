{ pkgs ? import <nixpkgs> {} }:
with pkgs;
mkShell {
  buildInputs = [
    (import ../default.nix { inherit pkgs; }).main
    pkgs.niv
  ];

  shellHook = ''
    ${import ./bad-1.nix {}}

    export ELINTER_CONFIG_ROOT="$(mktemp -d)"
    trap "rm -rf ''${ELINTER_CONFIG_ROOT}" 0 1 2 6 15
  
    echo "Testing dependency updating"
    ! [[ -f "''${ELINTER_CONFIG_ROOT}/nix/sources.json" ]]
    elinter -u -c
    [[ -f "''${ELINTER_CONFIG_ROOT}/nix/sources.json" ]]
    orig=$(sha1sum ./nix/sources.json | cut -d' ' -f1)
    new=$(sha1sum "''${ELINTER_CONFIG_ROOT}/nix/sources.json" | cut -d' ' -f1)
    [[ "$orig" != "$new" ]]

    exit
  '';
}
