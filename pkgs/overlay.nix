{ inputs }:
final: prev:
let
  inherit (prev) lib;
  inherit (inputs.gitignore.lib) gitignoreSource;

  pkgs = lib.composeManyExtensions [
    (import (inputs.emacs-ci + "/overlay.nix"))
    inputs.twist.overlay
  ] final prev;
in {
  elinter = lib.makeScope prev.newScope (self: {
    emacs = pkgs.emacs-snapshot;

    emacsCIVersions = lib.getAttrs pkgs.emacs-ci-versions pkgs;

    emacsConfigForLint = self.callPackage ./emacs-config {
      inherit (pkgs) emacsTwist;
      inherit inputs;

      extraPackages = [
        "package-lint"
        "elsa"
      ];
      lockDir = ./emacs-config/lock;
      # Allow the user to update lint packages
      inputOverrides = {
        package-lint = _: _: {
          src = inputs.package-lint;
        };
        elsa = _: _: {
          src = inputs.elsa;
        };
      };
    };

    elsa = self.callPackage ./elsa {
      emacsWithElsa = self.emacsConfigForLint;
    };

    elinter = lib.makeOverridable (self.callPackage ./elinter { }) {
      # TODO: Use a proper module API
      plugins = {
        package-lint = self.callPackage ./plugins/package-lint {
          inherit (self.emacsConfigForLint.elispPackages) package-lint;
        };
        byte-compile-and-load = self.callPackage ./plugins/byte-compile { };
        elsa = self.callPackage ./plugins/elsa { };
      };

      enabledPlugins = [ "package-lint" "byte-compile-and-load" ];
    };

    mkEmacsConfigForDevelopment =
      { src, lockDirName
      , localPackages, extraPackages
      , emacs ? self.emacs
      , compile ? false
      }:
      self.callPackage ./emacs-config ({
        inherit (pkgs) emacsTwist;
        inherit inputs;
        inherit emacs;

        extraPackages = localPackages ++ extraPackages;
        lockDir = src + "/${lockDirName}";
        # Allow the user to update lint packages
        inputOverrides = lib.genAttrs localPackages (_: _: _: {
          src = gitignoreSource src;
        });
      } // lib.optionalAttrs compile {
        elispPackageOverrides = _eself: esuper: 
          lib.genAttrs localPackages (ename: esuper.${ename}.overrideAttrs (_: {
            dontByteCompile = false;
            errorOnWarn = true;
          }));
      }
      );
  });
}
