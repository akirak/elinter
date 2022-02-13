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
    emacs = pkgs.emacs-27-2;

    emacsConfigForLint = self.callPackage ./emacs-config {
      inherit (pkgs) emacsTwist;
      inherit inputs;

      extraPackages = [ "package-lint" ];
      lockDir = ./emacs-config/lock;
      # Allow the user to update lint packages
      inputOverrides = {
        package-lint = _: _: {
          src = inputs.package-lint;
        };
      };
    };

    elinter = self.callPackage ./elinter {
      inherit (self.emacsConfigForLint.elispPackages) package-lint;
    };

    mkEmacsConfigForDevelopment = { src, lockDirName, localPackages }:
      self.callPackage ./emacs-config {
        inherit (pkgs) emacsTwist;
        inherit inputs;

        extraPackages = localPackages;
        lockDir = src + "/${lockDirName}";
        # Allow the user to update lint packages
        inputOverrides = lib.genAttrs localPackages (_: _: _: {
          src = gitignoreSource src;
        });
      };
  });
}
