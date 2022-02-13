{ inputs
}:
{ system
  # Package configuration
, src
, lockDirName ? "lock"
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

  admin = emacsConfig.admin lockDirName;

  update = pkgs.writeShellScriptBin "update" ''
    set -euo pipefail

    nix flake update
    ${admin.update}/bin/lock
    cd ${lockDirName}
    nix flake update
  '';

  elinter = pkgs.callPackage ../pkgs/elinter {
    inherit emacs inputs;
  };
in
{
  packages = {
    inherit emacsConfig;
    inherit (admin) lock;
    inherit update;
    inherit elinter;
  };

  inherit elispPackages;
}
