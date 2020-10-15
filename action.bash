#!/usr/bin/env bash

err() { echo "$*" >&2; }

set -euo pipefail

# shellcheck disable=SC1090
. "$(dirname "$0")/share/workflow.bash"

trap 'exit 1' 1 2 6 15

# Close the initial group
workflow_end_group

workflow_start_group "Enable binary cache of Emacs"
if ! command -v cachix >/dev/null; then
  nix-env -iA cachix -f https://cachix.org/api/v1/install
fi
cachix use emacs-ci
workflow_end_group

workflow_start_group "Install elinter"
if ! command elinter >/dev/null; then
  nix-env -if https://github.com/akirak/elinter/archive/v4.tar.gz
fi
workflow_end_group

echo

export ELINTER_LINTERS="${ELINTER_LINTERS:-checkdoc check-declare package-lint melpazoid}"

elinter -e all
