config@{ pkgs }:
with builtins;
let
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
    in if match "^[[:space:]]*$" raw != null then
      [ ]
    else
      filter (str: pathExists (rootDir + "/${str}")) (splitString " " raw);

  libs = foldl' (a: b: a // b) { } (map (file: import file config) [
    ./package.nix
    ./dhall.nix
    ./emacs.nix
    ./tests.nix
  ]);
in { inherit concatShArgs discoverFiles; } // libs
