{ elinterEnv
, elinterLib
, gnugrep
, makeWrapper
, runCommandNoCC
}:
let
  emacsForLint = (
    import ../emacsForCI.nix {
      version = elinterLib.latestStableEmacsVersion;
      elispPackages =
        elinterLib.excludeBuiltinElispPackages elinterEnv.linters
        ++ [ "package-build" ];
      libNix = ../lib.nix;
    }
  ).package;
in
runCommandNoCC "elinter-file-linter"
{
  preferLocalBuild = true;

  propagateBuildInputs = [
    emacsForLint
    elinterEnv.share
  ];

  buildInputs = [
    makeWrapper
  ];
}
  ''
    mkdir -p $out/bin
    makeWrapper ${emacsForLint}/bin/emacs $out/bin/elinter-lint-files \
      --run "exec &> >(${gnugrep}/bin/grep -E -f ${elinterEnv.share}/share/elinter/file-linter-patterns.txt)" \
      --add-flags "-Q --batch" \
      --add-flags "--script ${elinterEnv.share}/share/elinter/elinter-run-linters.el" \
      --set-default ELINTER_LINTERS "${elinterEnv.lintersAsString}" \
      --set-default ELINTER_LINT_CUSTOM_FILE ${elinterEnv.share}/share/elinter/lint-options.el
  ''
