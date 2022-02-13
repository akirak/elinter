{ nixpkgs
, overlay
}:
{ system
  # Package configuration
, src
, lockDirName ? "lock"
, localPackages
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
    inherit src lockDirName localPackages;
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
