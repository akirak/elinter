#!/usr/bin/env bash

set -euo pipefail

root="$PWD/.elinter/build"

build_package() {
  local name="$1"
  local return_code=0
  echo "Installing dependencies for $name..."
  local sandbox="$PWD/../../sandboxes/$name"
  emacs -Q --batch \
        --eval "(setq user-emacs-directory (expand-file-name emacs-version \"$sandbox\"))" \
        -l elinter-install-deps.el >/dev/null
  for f in *.el; do
    [[ "$f" = *-pkg.el ]] && continue
    echo "Compiling $f..."
    if ! emacs -Q --batch \
         --eval "(setq user-emacs-directory (expand-file-name emacs-version \"$sandbox\"))" \
         -l package \
         --eval "(package-initialize)" \
         -L . \
         -f batch-byte-compile "$f"
    then
      return_code="$?"
    fi
  done
  rm -f *.elc
  return "${return_code}"
}

failed_packages=()

build_packages() {
  cd "$root"
  for dir in *
  do
    [[ ! -d "$dir" ]] && continue

    cd "$dir"
    if ! build_package "$dir"
    then
      failed_packages+=($dir)
    fi
    cd "$root"
  done
  if [[ ${#failed_packages[@]} -gt 0 ]]; then
    return 1
  fi
}

build_packages
