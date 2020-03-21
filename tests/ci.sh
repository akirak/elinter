#!/usr/bin/env bash

set -e

EMACS="${EMACS:-emacs}"

nix-build() {
    command nix-build --quiet "$@"
}

nix-shell() {
    command nix-shell --pure --quiet "$@"
}

# nix-build -A byte-compile
# # result directory should be empty
# [[ `ls result` = "" ]]
# # Clean up
# rm -rf result

nix-build -A checkdoc
# There should be an empty checkdoc.log file in result directory
[[ `stat --printf=%s result/checkdoc.log` = "0" ]]
# Clean up
rm -rf result

nix-build -A melpaBuild
# Clean up
rm -rf result

nix-shell -A package-lint

nix-shell -A buttercup

# The following commands are expected to fail.
! nix-build -A byte-compile bad-hello.nix
! nix-build -A checkdoc bad-hello.nix
! nix-shell -A package-lint bad-hello.nix
