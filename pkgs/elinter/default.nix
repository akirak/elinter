{ writeShellApplication
, writeShellScriptBin
, writers
, jq
, lib
, plugins
, enabledPlugins ? [ "elsa" "package-lint" "byte-compile-and-load" ]
}:
with builtins;
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
        for cmd in ${lib.escapeShellArgs enabledPlugins}
        do
          if ! $cmd
          then
            r=1
          fi
        done
        exit $r
      fi
    '')
  ] ++ lib.attrVals enabledPlugins plugins;

  text = readFile ./lint.bash;
}
