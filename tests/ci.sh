#!/usr/bin/env bash

set -e

EMACS="${EMACS:-emacs}"

nix-build() {
    command nix-build --quiet --no-build-output "$@"
}

nix-shell() {
    env NIX_BUILD_SHELL=bash nix-shell --pure --quiet --no-build-output "$@"
}

nix-shell -A checkdoc

# nix-build -A byte-compile --no-out-link

nix-shell -A package-lint.hello
nix-shell -A package-lint.hello2

# nix-shell -A buttercup
# nix-shell -A buttercup --arg testDir ./tests/.

# The following commands are expected to fail.
# Temporarily disable these checks.

! nix-shell -A checkdoc bad.nix

# ! nix-build -A byte-compile --no-out-link bad.nix

! nix-shell -A package-lint.bad-hello bad.nix

echo "All tests have passed."
