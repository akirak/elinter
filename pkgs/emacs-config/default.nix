{ emacs, inputs, emacsTwist, inputOverrides, extraPackages, lockDir }:
let
  inherit (inputs.gitignore.lib) gitignoreSource;
in
(emacsTwist {
  emacsPackage = emacs;
  inventories = import ./inventories.nix {
    inherit (inputs) gnu-elpa melpa;
  };
  initFiles = [ ];
  inherit inputOverrides extraPackages lockDir;
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
