{ inputs, overlay }:
let
  inherit (inputs) flake-utils;

  mkFlakeForSystem = import ./mkFlakeForSystem.nix {
    inherit (inputs) nixpkgs;
    inherit overlay;
  };

  mkFlake =
    { systems ? flake-utils.lib.defaultSystems
    , ...
    } @ args: flake-utils.lib.eachSystem systems (system:
    mkFlakeForSystem ({
      inherit system;
    } // builtins.removeAttrs args [ "systems" ])
    );
in
{
  inherit mkFlakeForSystem mkFlake;
}
