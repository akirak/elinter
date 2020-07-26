{ pkgs ? import <nixpkgs> {}
, src
, recipe
}:
with builtins;
with pkgs;
with (import (import ./nix/sources.nix).nix-elisp-helpers { inherit pkgs; });
with (import (import ./nix/sources.nix).gitignore { inherit (pkgs) lib; });
let
  srcRoot = gitignoreSource (/. + "/${src}");
  package =
    let
      recipeAttrs = parseRecipe recipe;
      isElisp = file: match ".+\.el" file != null;
      mainFileRegex = "(.*/)?" + recipeAttrs.pname + "\.el";
      isMainFile = file: match mainFileRegex file != null;
    in
      rec {
        inherit (recipeAttrs) pname;
        packageFiles = expandPackageFiles srcRoot recipeAttrs.files;
        sourceFiles = filter isElisp packageFiles;
        mainFile =
          if length sourceFiles == 1
          then head sourceFiles
          else head (filter isMainFile sourceFiles);
      };
in
  with package;
  let
    packageFilePaths = map (file: src + "/" + file) packageFiles;
    baseNames = xs: concatStringsSep " " (map baseNameOf xs);
  in
    writeText "emacs-${package.pname}-link-source"
      ''
        cat > .envrc <<HERE
        export PACKAGE_NAME=${pname}
        export PACKAGE_FILES="${baseNames packageFiles}"
        export PACKAGE_ELISP_FILES="${baseNames sourceFiles}"
        export PACKAGE_MAIN_FILE=${baseNameOf mainFile}
        HERE
        for f in ${lib.escapeShellArgs packageFilePaths}; do
          ln -s $f
        done
      ''
