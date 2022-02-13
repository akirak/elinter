{ writers, elsa }:
writers.writeBashBin "elsa" ''
  set -euo pipefail

  tmpdir=$(mktemp -t -d emacs-elsaXXX)
  origdir=$(pwd)
  cleanup() {
    cd "$origdir" && rm -rf "$tmpdir"
  }
  trap cleanup EXIT ERR

  for f in *.el
  do
    if [[ "$f" =~ (.+)-autoloads.el ]]
    then
      continue
    fi
    cp "$f" "$tmpdir"
  done

  cd "$tmpdir"

  echo "Running ELSA..."
  ${elsa}/bin/elsa *.el
''
