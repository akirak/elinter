{ writeShellApplication
, writeShellScriptBin
, writers
, jq
, emacs
, emacsTwist
, inputs
}:
with builtins;
let
  config = import ./emacs.nix {
    inherit emacs emacsTwist inputs;
  };

  inherit (config.elispPackages) package-lint;
in
writeShellApplication {
  name = "elinter";

  runtimeInputs = [
    jq

    (writeShellScriptBin "with-package-lisp" ''
      cd "$1" || exit 1
      shift

      if [[ $# -gt 0 ]]
      then
        exec "$@"
      else
        r=0
        for cmd in package-lint byte-compile-and-load
        do
          if ! $cmd
          then
            r=1
          fi
        done
        exit $r
      fi
    '')

    (writers.writeBashBin "package-lint" ''
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

      echo "Checking the package with package-lint..."
      set -x
      emacs -batch -L "${package-lint}/share/emacs/site-lisp" -l package-lint \
        --eval "(setq package-lint-main-file \"''${main_file}\")" \
        -l ${./package-lint-init.el} \
        -f package-lint-batch-and-exit \
        ''${files[@]}
    '')

    (writers.writeBashBin "byte-compile-and-load" ''
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
    '')

  ];

  text = readFile ./lint.bash;
}
