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
