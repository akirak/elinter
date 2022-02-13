{ writers }:
writers.writeBashBin "byte-compile-and-load" ''
  set -euo pipefail

  tmpdir=$(mktemp -t -d emacs-byte-compileXXX)
  origdir=$(pwd)
  cleanup() {
    cd "$origdir" && rm -rf "$tmpdir"
  }
  trap cleanup EXIT ERR

  for f in *.el
  do
    if [[ "$f" =~ (.+)-autoloads.el ]]
    then
      ename="''${BASH_REMATCH[1]}"
      continue
    fi
    cp "$f" "$tmpdir"
  done

  cd "$tmpdir"
  status=0

  for f in *.el
  do
    echo "Compiling $f..."
    if ! emacs -batch --no-site-file -L . \
      --eval "(setq byte-compile-error-on-warn t)" \
      -f batch-byte-compile "$f"
    then
      status=1
    fi
  done

  if [[ $status -eq 0 ]]
  then
    echo "Loading $ename.elc..."
    if ! emacs -batch --no-site-file -L . -l "$ename.elc"
    then
      status=1
    fi
  fi
  exit $status
''
