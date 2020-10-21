{ pkgs ? import <nixpkgs> {}
, src
, recipeFile
}:
with builtins;
with pkgs;
with (import (import ./sources.nix).nix-elisp-helpers { inherit pkgs; });
with (import (import ./sources.nix).gitignore { inherit (pkgs) lib; });
let
  srcRoot = gitignoreSource (/. + "/${src}");
  package =
    let
      recipeAttrs = parseRecipe (readFile recipeFile);
      isElisp = file: match ".+\.el" file != null;
      mainFileRegex = "(.*/)?" + recipeAttrs.pname + "\.el";
      isMainFile = file: match mainFileRegex file != null;
    in
      rec {
        inherit (recipeAttrs) pname;
        packageFiles =
          filter (file: match "(.*/)?flycheck_.+\.el" file == null)
            (expandPackageFiles srcRoot recipeAttrs.files);
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
        cat > .elinter-env <<HERE
        export PACKAGE_NAME=${pname}
        export PACKAGE_FILES="${baseNames packageFiles}"
        export PACKAGE_ELISP_FILES="${baseNames sourceFiles}"
        export PACKAGE_MAIN_FILE=${baseNameOf mainFile}
        HERE
        for f in ${lib.escapeShellArgs packageFilePaths}; do
          if ! ln -s $f; then
            echo "This is possibly due to duplicate file specs in the recipe." >&2
            exit 1
          fi
        done
      ''
