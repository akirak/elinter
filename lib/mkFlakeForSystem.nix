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

  emacsVersions = import ./versionMatrix.nix {
    inherit lib;
    inherit localPackages;
    inherit (emacsConfig) packageInputs;
    inherit emacsCIVersions;
  };

  admin = emacsConfig.admin lockDirName;

  update = pkgs.writeShellScriptBin "update" ''
    set -euo pipefail

    nix flake lock --update-input elinter
    ${admin.update}/bin/lock
    cd ${lockDirName}
    nix flake update
  '';

  makeScriptDerivation = { name, emacs, text }: pkgs.writeShellApplication {
    inherit name;
    runtimeInputs = [
      emacs
    ];
    inherit text;
  };

  scriptPackages = lib.mapAttrs
    (prefix: { text, compile ? false }:
      lib.extendDerivation true
        {
          matrix = lib.genAttrs emacsVersions (emacsVersion: makeScriptDerivation {
            name = "${prefix}-${emacsVersion}";
            emacs = emacsConfig.override {
              emacs = emacsCIVersions.${emacsVersion};
              inherit compile;
            };
            inherit text;
          });
        }
        (makeScriptDerivation {
          name = prefix;
          emacs = emacsConfig.override { inherit compile; };
          inherit text;
        })
    ) scripts;
in
{
  packages = {
    inherit emacsConfig;
    emacsConfig27 = emacsConfig.override { emacs = pkgs.elinter.emacsCIVersions.emacs-27-2; };
    inherit (admin) lock;
    inherit update;
    inherit (pkgs.elinter) elinter;
  } // scriptPackages;

  inherit elispPackages;
}
