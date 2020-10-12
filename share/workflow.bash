gh_workflow_p() {
  [[ -v GITHUB_ACTIONS && "${GITHUB_ACTIONS}" = true ]]
}

if gh_workflow_p; then
  workflow_start_group() {
    echo "::group ::$*"
  }

  workflow_end_group() {
    echo "::endgroup ::"
  }
else
  workflow_start_group() {
    return
  }

  workflow_end_group() {
    echo
  }
fi

workflow_with_group() {
  local r
  workflow_start_group "$1"
  shift
  "$@"
  r=$?
  workflow_end_group
  return $r
}

