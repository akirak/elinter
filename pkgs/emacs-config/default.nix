{ emacs
, lib
, inputs
, emacsTwist
, inputOverrides
, extraPackages
, lockDir
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
  inputOverrides = {
    buttercup = _: super: {
      files = builtins.removeAttrs super.files [ "buttercup-pkg.el" ];
    };
  } // inputOverrides;
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
