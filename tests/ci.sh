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
nix-build -A prepareButtercup.hello --no-build-output
nix-build -A prepareButtercup.hello2 --no-build-output
nix-shell -A buttercup.hello
nix-shell -A buttercup.hello2
nix-build ert -A prepareErt.hello3
nix-shell ert -A ert.hello3

# The following commands are expected to fail.
! nix-shell bad.nix -A meta
! nix-shell bad.nix -A checkdoc
! nix-build bad.nix -A byte-compile
! nix-shell bad.nix -A package-lint.bad-hello
nix-build ert -A prepareErt.hello4
! nix-shell ert -A ert.hello4

echo
echo "@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@"
echo "All checks have passed."
echo
