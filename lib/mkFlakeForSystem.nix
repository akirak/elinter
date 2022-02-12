{ inputs
}:
{ system
  # Package configuration
, src
, lockDirName
, localPackages
}:
with builtins;
let
  inherit (inputs) nixpkgs;
  inherit (nixpkgs) lib;
  inherit (inputs.gitignore.lib) gitignoreSource;

  pkgs = import nixpkgs {
    inherit system;
    overlays = [
      (import (inputs.emacs-ci + "/overlay.nix"))
      inputs.twist.overlay
    ];
  };

  emacs = pkgs.callPackage ./emacsSmall.nix { };

  emacsConfig = pkgs.callPackage ./config.nix {
    inherit emacs;
    inherit localPackages;
    src = gitignoreSource src;
    inventories = import ./inventories.nix {
      inherit (inputs) melpa gnu-elpa;
    };
    lockDir = src + "/${lockDirName}";
  };

  elispPackages = lib.getAttrs localPackages emacsConfig.elispPackages;

  lintConfig = pkgs.callPackage ./emacs.nix {
    inherit inputs;
  };

  elinter = pkgs.callPackage ../pkgs/elinter {
    inherit emacs inputs;
  };
in
{
  packages = {
    inherit emacsConfig;
    inherit (emacsConfig.admin lockDirName) update lock;
    inherit elinter;
  };

  inherit elispPackages;
}
