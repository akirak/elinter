gh_workflow_p() {
  [[ -v GITHUB_ACTIONS && "${GITHUB_ACTIONS}" = true ]]
}

workflow_start_group() {
  if gh_workflow_p; then
    echo "::group ::$*"
  fi
}

workflow_end_group() {
  if gh_workflow_p; then
    echo "::endgroup ::"
  else
    echo
  fi
}

workflow_with_group() {
  local r
  workflow_start_group "$1"
  shift
  "$@"
  r=$?
  workflow_end_group
  return $r
}
