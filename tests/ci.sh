#!/usr/bin/env bash

set -e

EMACS="${EMACS:-emacs}"

export NIX_BUILD_SHELL=bash

nix-build() {
    command nix-build --quiet "$@"
}

nix-shell() {
    command nix-shell --pure --quiet "$@"
}

nix-build -A checkdoc 
# There should be an empty checkdoc.log file in result directory
[[ `stat --printf=%s result/checkdoc.log` = "0" ]]
# Clean up
rm -rf result

nix-build -A byte-compile
# Clean up
rm -rf result

nix-shell -A package-lint.hello
nix-shell -A package-lint.hello2

nix-shell -A buttercup
nix-shell -A buttercup --arg testDir ./tests/.

# The following commands are expected to fail.
# Temporarily disable these checks.
! nix-build -A checkdoc bad.nix
! nix-build -A byte-compile bad.nix
! nix-shell -A package-lint.bad-hello bad.nix

echo "All tests have passed."
