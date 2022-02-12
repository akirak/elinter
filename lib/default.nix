{ inputs }:
let
  inherit (inputs) flake-utils;

  mkFlakeForSystem = import ./mkFlakeForSystem.nix {
    inherit inputs;
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
