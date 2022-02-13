{ emacs
, lib
, inputs
, emacsTwist
, inputOverrides
, extraPackages
, lockDir
, compile ? false
, elispPackageOverrides ? _: esuper: esuper
}:
let
  inherit (inputs.gitignore.lib) gitignoreSource;
in
(emacsTwist {
  emacsPackage = emacs;
  inventories = import ./inventories.nix {
    inherit (inputs) gnu-elpa melpa;
  };
  initFiles = [ ];
  inherit extraPackages lockDir;
  # TODO: Allow composing overrides
  inputOverrides = (import ./workarounds.nix) // inputOverrides;
}).overrideScope' (_self: super: {
  elispPackages = super.elispPackages.overrideScope' (
    lib.composeExtensions
      (eself: esuper:
        builtins.mapAttrs
          (ename: epkg:
            epkg.overrideAttrs (_: {
              dontByteCompile = true;
            })
          )
          esuper)
      elispPackageOverrides
  );
})
