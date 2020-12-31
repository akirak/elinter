{ elinterEnv
, makeWrapper
}:
runCommandNoCC "elinter-linters"
{
  preferLocalBuild = true;
  inherit (elinterEnv) src;
  buildInputs = [
    makeWrapper
  ];
  propagateBuildInputs = [
    elinterEnv.share
  ];
}
  ''
    mkdir -p $out/bin
    for f in elinter-logger elinter-github-logger \
        elinter-byte-compile elinter-run-linters; do
      cp $src/bin/$f $out/bin
      chmod +x $out/bin/$f
    done
    substituteInPlace $out/bin/elinter-run-linters \
      --replace share/workflow.bash ${elinterEnv.share}/share/elinter/workflow.bash \
      --replace share/lint-options.el ${elinterEnv.share}/share/elinter/lint-options.el \
      --replace share/elinter-run-linters.el ${share}/share/elinter/elinter-run-linters.el
    substituteInPlace $out/bin/elinter-byte-compile \
      --replace share/workflow.bash ${elinterEnv.share}/share/elinter/workflow.bash
    substituteInPlace $out/bin/elinter-github-logger \
      --replace share/github-log.sed ${elinterEnv.share}/share/elinter/github-log.sed

    # Patch backend scripts to redirect output to the logger
    for backend in elinter-run-linters elinter-byte-compile; do
      wrapProgram $out/bin/$backend --run "exec &> >(elinter-logger)"
    done
  ''
