# Download a remote source based on recipe
{ pkgs ? import <nixpkgs> {}
, recipeFile
}:
(import (import ./sources.nix).nix-elisp-helpers { inherit pkgs; }).fetchFromRecipe
  (builtins.readFile recipeFile);
