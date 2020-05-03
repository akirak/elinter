{ pkgs, emacsDerivation }:
package:
with (import ../lib);
assert (builtins.isPath package.src);
assert (builtins.pathExists package.src);
assert (builtins.all
  (file: let srcPath = package.src + "/${file}"; in builtins.pathExists srcPath)
  package.files);
pkgs.stdenv.mkDerivation {
  name = package.pname + "-checkdoc";
  buildInputs = [ emacsDerivation pkgs.coreutils ];
  shellHook = ''
    echo
    echo ==========================================================
    echo Checkdoc on ${package.pname} package
    echo ==========================================================
    cd ${package.src}
    emacs --batch --no-site-file \
          --load ${./checkdoc-runner.el} \
          ${concatShArgs package.files}
    exit $?
  '';
}
