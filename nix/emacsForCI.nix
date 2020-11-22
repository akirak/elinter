{ version
, elispPackages
, libNix
, pkgs ? import <nixpkgs> {}
}:
rec {
  package = (import libNix).emacsDerivation {
    emacs =
      if
        version == "snapshot"
      then
        "emacs-snapshot"
      else
        "emacs-" + builtins.replaceStrings [ "." ] [ "-" ] version;

    dependencies = elispPackages;
  };

  shell = pkgs.mkShell {
    buildInputs = [ package ];
  };
}
