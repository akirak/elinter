#!/usr/bin/env bash

err() { echo "$*" >&2; }

r=0

# Install cachix first
if ! command -v cachix >/dev/null; then
  nix-env -iA cachix -f https://cachix.org/api/v1/install
  cachix use emacs-ci
fi

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
