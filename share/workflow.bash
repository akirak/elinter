gh_workflow_p() {
  [[ -v GITHUB_ACTIONS && "${GITHUB_ACTIONS}" = true ]]
}

if gh_workflow_p; then
  workflow_start_group() {
    echo "::group ::$*"
    workflow_start_time=$(date +%s.%N)
  }

  workflow_end_group() {
    if [[ -v workflow_start_time ]]; then
      echo "$(date +%s.%N)" - "${workflow_start_time}" \
        | bc \
        | xargs printf "%.2f s\n"
    fi
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

