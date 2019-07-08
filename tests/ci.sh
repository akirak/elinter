#!/bin/bash

set -e

EMACS="${EMACS:-emacs}"

nix-build() {
    command nix-build --quiet \
            --arg emacs "(import <nixpkgs> {}).$EMACS" "$@"
}

nix-shell() {
    command nix-shell --pure --quiet \
            --arg emacs "(import <nixpkgs> {}).$EMACS" "$@"
}

nix-build -A byte-compile
# result directory should be empty
[[ `ls result` = "" ]]
# Clean up
rm -rf result

nix-build -A checkdoc
# There should be an empty checkdoc.log file in result directory
[[ `stat --printf=%s result/checkdoc.log` = "0" ]]
# Clean up
rm -rf result

nix-shell -A package-lint

# The following commands are expected to fail.
! nix-build -A byte-compile bad-hello.nix
! nix-build -A checkdoc bad-hello.nix
! nix-shell -A package-lint bad-hello.nix
