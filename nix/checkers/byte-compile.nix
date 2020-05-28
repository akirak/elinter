{ pkgs, customEmacsPackages, ... }:
package:
with (import ../lib);
assert (builtins.isString package.pname);
assert (builtins.isPath package.src);
assert (builtins.pathExists package.src);
assert (builtins.all
  (file: let srcPath = package.src + "/${file}"; in builtins.pathExists srcPath)
  package.files);
derivation {
  inherit (package) src;
  system = builtins.currentSystem;
  name = package.pname + "-byte-compile";
  builder = "${pkgs.bash}/bin/bash";
  buildInputs =
    [ pkgs.coreutils (customEmacsPackages.emacsWithPackages package.dependencies) ];
  args = [ ./byte-compile.sh ];
  # Only used in the shell script
  files = concatShArgs package.files;
  inherit (package) pname;
  dependencyNames = concatShArgs (package.dependencyNames or [ "unknown" ]);
  # localDependencyNames = concatShArgs package.localDependencyNames;
  loadPaths = let
    dependencies = package.dependencies pkgs.emacsPackages;
    dirs = (pkgs.lib.unique (map builtins.dirOf package.files));
    dquote = file: ''"'' + file + ''"'';
  in "'(${concatShArgs (map dquote dirs)})";
}

