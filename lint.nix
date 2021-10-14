{ runCommandNoCC
, makeWrapper
, emacsPackagesFor
, writeText
, src
}:
{ package
, recipeDir
, # https://github.com/riscy/melpazoid/blob/master/melpazoid/melpazoid.el
  customFile ? writeText "options.el" ''
    (setq sentence-end-double-space nil
          checkdoc-proper-noun-list nil
          checkdoc-common-verbs-wrong-voice nil)
  ''
}:
let
  emacsPackages = emacsPackagesFor package;
  emacs = emacsPackages.emacsWithPackages (epkgs: with epkgs; [
    package-lint
    package-build
  ]);
in
runCommandNoCC "elinter-lint"
{
  preferLocalBuild = true;
  nativeBuildInputs = [
    makeWrapper
  ];
  propagatedBuildInputs = [
    src
  ];
} ''
  mkdir -p $out/bin
  makeWrapper ${emacs}/bin/emacs $out/bin/elinter-lint \
    --set ELINTER_LINT_CUSTOM_FILE "${customFile}" \
    --set ELINTER_RECIPE_DIR "${recipeDir}" \
    --add-flags "-Q --batch" \
    --add-flags "--script ${src}/elinter-lint.el"
''
