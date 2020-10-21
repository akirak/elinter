{ pkgs ? import <nixpkgs> {}
, recipeFile
}:
with builtins;
with pkgs;
with (import (import ./sources.nix).nix-elisp-helpers { inherit pkgs; });
with (parseRecipe (readFile recipeFile));
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
    then url
    else null;
  srcStore =
    if gitUrl != null
    then fetchGit (
      repoAttrs // {
        name = pname;
        url = gitUrl;
      }
    )
    else if fetcher == "hg"
    then fetchMercurial (
      repoAttrs // {
        name = pname;
        inherit url;
      }
    )
    else throw ("Unsupported fetcher type: " + fetcher);
in
toString (lib.toDerivation srcStore)
