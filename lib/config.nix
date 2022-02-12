{ lib
, emacs
, emacsTwist
, localPackages
, src
, inventories
, lockDir
}:
with builtins;
(emacsTwist {
  inherit inventories;
  inherit lockDir;
  initFiles = [ ];
  extraPackages = localPackages;
  inputOverrides = lib.genAttrs localPackages (_: _: _: {
    inherit src;
  });
}).overrideScope' (_self: super: {
  elispPackages = super.elispPackages.overrideScope' (eself: esuper:
    mapAttrs
      (ename: epkg:
        epkg.overrideAttrs (_: {
          dontByteCompile = true;
        })
      )
      esuper
  );
})
