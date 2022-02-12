#!/usr/bin/env bash

usage() {
    echo "Usage: elinter PACKAGE [COMMAND] [ARGS]..."
}

flake=$(pwd)
nix_system=$(nix eval --expr builtins.currentSystem --impure --raw)

run_command() {
    local epkg_name="$1"
    shift
    drv="$flake#elispPackages.${nix_system}.${epkg_name}"
    out=$(nix build "$drv" --no-link --json | jq -r '.[0] | .outputs | .out')
    nix develop "$drv" -c with-package-lisp "$out/share/emacs/site-lisp" "$@"
}

if [[ $# -eq 0 ]]
then
    usage
    exit 200
fi

for arg
do
    case "$arg"
    in
        --help)
            usage
            exit 0
            ;;
        *)
            break
            ;;
    esac
done

package="$1"
shift
run_command "$package" "$@"
