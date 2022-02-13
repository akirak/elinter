{ writers
, package-lint
}:
writers.writeBashBin "package-lint" ''
  set -euo pipefail

  autoloads=$(ls *-autoloads.el)

  if [[ "$autoloads" =~ (.+)-autoloads.el ]]
  then
    main_file="''${BASH_REMATCH[1]}.el"
  fi

  files=()
  for f in *.el
  do
    if [[ "$f" = *-autoloads.el ]]
    then
      continue
    fi
    files+=("$f")
  done

  emacs_dir="''${XDG_DATA_HOME:-$HOME/.local/share}/elinter"
  mkdir -p "''${emacs_dir}"

  echo "Checking the package with package-lint..."
  set -x
  emacs -batch -L "${package-lint}/share/emacs/site-lisp" -l package-lint \
    --eval "(setq user-emacs-directory \"''${emacs_dir}/\")" \
    --eval "(setq package-lint-main-file \"''${main_file}\")" \
    -l ${./package-lint-init.el} \
    -f package-lint-batch-and-exit \
    ''${files[@]}
''
