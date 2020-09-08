let
  pkgs = import <nixpkgs> {};
  fetchRecipeSource = recipe: import ../nix/fetchSource.nix {
    inherit recipe;
  };
  recipe1 = ''
    (smex :repo "nonsequitur/smex" :fetcher github)
  '';
  recipe2 = ''
    (flymake-perlcritic :repo "illusori/emacs-flymake-perlcritic"
                        :fetcher github
                        :files ("*.el" ("bin" "bin/flymake_perlcritic")))
  '';
  recipe3 = ''
    (discover-my-major :fetcher git :url "https://framagit.org/steckerhalter/discover-my-major.git")
  '';
in
  assert (pkgs.lib.isStorePath (fetchRecipeSource recipe1));
  assert (pkgs.lib.isStorePath (fetchRecipeSource recipe2));
  assert (pkgs.lib.isStorePath (fetchRecipeSource recipe3));
  null
