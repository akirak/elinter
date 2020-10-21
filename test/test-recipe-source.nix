let
  pkgs = import <nixpkgs> {};

  fetchRecipeSource = recipeFile: import ../nix/fetchSource.nix {
    inherit recipeFile;
  };
in
  assert (pkgs.lib.isStorePath (fetchRecipeSource ./recipe1));
  assert (pkgs.lib.isStorePath (fetchRecipeSource ./recipe2));
  assert (pkgs.lib.isStorePath (fetchRecipeSource ./recipe3));
  null
