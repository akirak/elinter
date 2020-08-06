{ pkgs ? import <nixpkgs> {}
, src
, remote ? false
, recipe
}:
with builtins;
with pkgs;
with (import (import ./nix/sources.nix).nix-elisp-helpers { inherit pkgs; });
with (import (import ./nix/sources.nix).gitignore { inherit (pkgs) lib; });
let
  recipeAttrs = parseRecipe recipe;

  remoteSrc =
    with recipeAttrs;
    let
      path = split "/" repo;
      owner = elemAt path 0;
      repoPath = elemAt path 2;
      repoAttrs = lib.filterAttrs (_: v: v != null) {
        rev = commit;
        ref = branch;
      };
      gitUrl =
        if fetcher == "github"
        then concatStringsSep "" [ "https://github.com/" owner "/" repoPath ".git" ]
        else if fetcher == "gitlab"
        then concatStringsSep "" [ "https://gitlab.com/" owner "/" repoPath ".git" ]
        else if fetcher == "git"
        then recipeAttrs.url or throw "url is required when you choose the git fetcher"
        else null;
      srcStore = if gitUrl != null
      then fetchGit (
        repoAttrs // {
          name = pname;
          url = gitUrl;
        }
      )
      else if fetcher == "hg"
      then fetchMercurial (
        repoAttrs // {
          inherit url;
        }
      )
      else throw ("Unsupported fetcher type: " + fetcher);
    in
      lib.toDerivation srcStore;

  srcRoot = if remote
  then remoteSrc
  else gitignoreSource (/. + "/${src}");

  package =
    let
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
    baseDir = if remote then toString remoteSrc else src;
    packageFilePaths = map (file: baseDir + "/" + file) packageFiles;
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
