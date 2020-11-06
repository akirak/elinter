{ pkgs ? import <nixpkgs> {} }:
with pkgs;
mkShell {
  buildInputs = [
    (import ../default.nix { inherit pkgs; })

    pkgs.niv
  ];

  shellHook = ''
    # To make CI pass, prevent error/warning annotations on GitHub Actions
    export ELINTER_NO_ANNOTATE_GITHUB=1

    ${import ./bad-1.nix {}}

    export ELINTER_CONFIG_ROOT="$(mktemp -d)"
    trap "rm -rf ''${ELINTER_CONFIG_ROOT}" 0 1 2 6 15

    # Niv doesn't seem to correctly update dependencies on
    # GitHub Actions, so I will disable this check.
    # TODO: Remove this comment and re-enable niv updates
    if [[ -v GITHUB_ACTIONS ]]; then
      exit 0
    fi

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
