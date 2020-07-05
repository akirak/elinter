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
nix-shell -A checkdoc.default
nix-build -A byte-compile.default
nix-shell -A package-lint.hello
nix-shell -A package-lint.hello2
nix-build -A prepareLoad.hello --no-build-output
nix-shell -A load.hello
nix-build -A prepareLoad.hello2 --no-build-output
nix-shell -A load.hello2
# nix-build -A prepareButtercup.hello --no-build-output
nix-shell -A buttercup.hello
# nix-build -A prepareAllTests.hello2 --no-build-output
nix-shell -A allTests.hello2
# nix-build ert -A prepareErt.hello3
nix-shell ert -A ert.hello3

# The following commands are expected to fail.
! nix-shell bad.nix -A meta
! nix-shell bad.nix -A checkdoc.default
! nix-build bad.nix -A byte-compile.default
! nix-shell bad.nix -A package-lint.bad-hello
! nix-build bad.nix -A prepareLoad.bad-hello --no-build-output
! nix-shell bad.nix -A load.bad-hello
# nix-build ert -A prepareErt.hello4
! nix-shell ert -A ert.hello4

echo
echo "@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@"
echo "All checks have passed."
echo
