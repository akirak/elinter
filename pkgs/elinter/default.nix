{ writeShellApplication
, writeShellScriptBin
, jq
, lib
}:
{ plugins
, enabledPlugins
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
