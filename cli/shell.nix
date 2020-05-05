let pkgs = import ../nix/pkgs.nix;
in with pkgs; mkShell { buildInputs = [ spago nodejs purs gnumake ]; }
