with builtins;
let
  pkgs = import ../pkgs.nix;

  concatShArgs = files: pkgs.lib.foldr (a: b: a + " " + b) "" files;

  discoverFiles = rootDir: patterns:
    with pkgs.lib;
    let
      drv = pkgs.stdenv.mkDerivation {
        name = "bath-glob";
        buildInputs = [ pkgs.bash ];
        buildCommand = ''
          shopt -s extglob nullglob
          cd ${rootDir}
          echo ${concatShArgs patterns} > $out
        '';
      };
      raw = fileContents drv;
    in
      if builtins.match "^[[:space:]]*$" raw != null
      then []
      else filter (str: pathExists (rootDir + "/${str}")) (splitString " " raw);
in {
  inherit concatShArgs discoverFiles;
} // (import ./package.nix) // (import ./dhall.nix) // (import ./emacs.nix)
