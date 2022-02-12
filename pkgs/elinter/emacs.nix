{ emacs, inputs, emacsTwist }:
let
  inherit (inputs.gitignore.lib) gitignoreSource;
in
(emacsTwist {
  emacsPackage = emacs;
  inventories = import ../../lib/inventories.nix {
    inherit (inputs) gnu-elpa melpa;
  };
  lockDir = gitignoreSource ../../.;
  initFiles = [ ];
  extraPackages = [ "package-lint" ];
}).overrideScope' (_self: super: {
  elispPackages = super.elispPackages.overrideScope' (eself: esuper:
    builtins.mapAttrs
      (ename: epkg:
        epkg.overrideAttrs (_: {
          dontByteCompile = true;
        })
      )
      esuper
  );
})
