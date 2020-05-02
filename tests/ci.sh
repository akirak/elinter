#!/usr/bin/env bash

set -e

EMACS="${EMACS:-emacs}"

nix-build() {
    command nix-build --quiet --no-out-link "$@" --show-trace
}

nix-shell() {
    env NIX_BUILD_SHELL=bash nix-shell --pure --quiet "$@" --show-trace
}

nix-shell -A meta
nix-shell -A checkdoc
nix-build -A byte-compile
nix-shell -A package-lint.hello
nix-shell -A package-lint.hello2
nix-build -A prepareButtercup --no-build-output
nix-shell -A buttercup.hello
nix-shell -A buttercup.hello2

# The following commands are expected to fail.
! nix-shell bad.nix -A meta
! nix-shell bad.nix -A checkdoc
! nix-build bad.nix -A byte-compile
! nix-shell bad.nix -A package-lint.bad-hello

echo
echo "@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@"
echo "All checks have passed."
echo
