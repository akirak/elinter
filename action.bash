#!/usr/bin/env bash

err() { echo "$*" >&2; }

r=0

# shellcheck disable=SC1090
. "$(dirname "$0")/share/workflow.bash"

trap 'exit 1' 1 2 6 15

# Close the initial group
workflow_end_group

workflow_start_group "Install cachix"
if ! command -v cachix >/dev/null; then
  nix-env -iA cachix -f https://cachix.org/api/v1/install
fi
workflow_end_group

workflow_start_group "Enable nix-emacs-ci"
cachix use emacs-ci
workflow_end_group

workflow_start_group "Install elinter"
nix-env -if https://github.com/akirak/elinter/archive/v4.tar.gz -A main || exit 1
workflow_end_group

elinter -l -e snapshot --experimental || r=1

elinter -b -e all || r=1

if [[ -z "${ELINTER_ACTION_TEST}" ]]; then
  echo "No tests are specified. Skipping"
else
  case "${ELINTER_ACTION_TEST}" in
    buttercup)
      flags=("--buttercup")
      ;;
    ert-runner)
      flags=("--ert-runner")
      ;;
    *)
      err "Unsupported test type: $1"
      exit 1
      ;;
  esac
  shift
  # shellcheck disable=SC2086
  elinter -t "${flags[*]}" -e latest -- ${ELINTER_ACTION_TEST_ARGS} || r=1
fi

exit $r
