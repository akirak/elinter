let pkgs = import ../pkgs.nix;
in {
  dhallToNix = srcDir: file:
    let
      drv = pkgs.stdenv.mkDerivation {
        name = "generate-nix-from-dhall";

        buildCommand = ''
          cd ${srcDir}
          cd ${builtins.dirOf file}
          dhall-to-nix < "${builtins.baseNameOf file}" > $out
        '';

        buildInputs = [ pkgs.dhall-nix.dhall-nix-simple ];
      };
    in import "${drv}";

  dhallToJson = srcDir: file:
    pkgs.stdenv.mkDerivation {
      name = "generate-json-from-dhall";

      buildCommand = ''
        cd ${srcDir}
        cd ${builtins.dirOf file}
        dhall-to-json < "${builtins.baseNameOf file}" > $out
      '';

      buildInputs = [ pkgs.dhall-nix.dhall-json-simple ];
    };
}
