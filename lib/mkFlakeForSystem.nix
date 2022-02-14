{ nixpkgs
, overlay
}:
{ system
  # Package configuration
, src
, lockDirName ? "lock"
, localPackages
, extraPackages ? [ ]
, scripts ? { }
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

  inherit (pkgs.elinter) emacsCIVersions;

  emacsConfig = lib.makeOverridable pkgs.elinter.mkEmacsConfigForDevelopment {
    inherit src lockDirName localPackages extraPackages;
  };

  elispPackages = lib.getAttrs localPackages emacsConfig.elispPackages;

  maxVersion = versions: head (sort (a: b: compareVersions a b > 0) versions);

  minimumEmacsVersion = lib.pipe localPackages [
    (map (ename: emacsConfig.packageInputs.${ename}.packageRequires.emacs or null))
    (filter isString)
    maxVersion
  ];

  admin = emacsConfig.admin lockDirName;

  update = pkgs.writeShellScriptBin "update" ''
    set -euo pipefail

    nix flake lock --update-input elinter
    ${admin.update}/bin/lock
    cd ${lockDirName}
    nix flake update
  '';

  scriptPackages = lib.mapAttrs (pkgs.elinter.makeScriptPackage {
    inherit minimumEmacsVersion emacsConfig;
  })
    scripts;
in
{
  packages = {
    inherit emacsConfig;
    inherit (admin) lock;
    inherit update;
    inherit (pkgs.elinter) elinter;
  } // scriptPackages;

  inherit elispPackages;
}
