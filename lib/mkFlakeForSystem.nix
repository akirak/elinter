{ nixpkgs
, overlay
}:
{ system
  # Package configuration
, src
, lockDirName ? "lock"
, localPackages
, extraPackages ? [ ]
}:
with builtins;
let
  inherit (nixpkgs) lib;

  pkgs = import nixpkgs {
    inherit system;
    overlays = [
      overlay
    ];
  };

  emacsConfig = pkgs.elinter.mkEmacsConfigForDevelopment {
    inherit src lockDirName localPackages extraPackages;
  };

  elispPackages = lib.getAttrs localPackages emacsConfig.elispPackages;

  admin = emacsConfig.admin lockDirName;

  update = pkgs.writeShellScriptBin "update" ''
    set -euo pipefail

    nix flake lock --update-input elinter
    ${admin.update}/bin/lock
    cd ${lockDirName}
    nix flake update
  '';
in
{
  packages = {
    inherit emacsConfig;
    inherit (admin) lock;
    inherit update;
    inherit (pkgs.elinter) elinter;
  };

  inherit elispPackages;
}
