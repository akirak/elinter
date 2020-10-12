{ sources ? null, epkgs, lib }: linters:
with builtins;
let
  melpazoidSource = import ./sourceWithFallback.nix sources "melpazoid";
in
(
  lib.optional (elem "melpazoid" linters) (
    epkgs.trivialBuild {
      pname = "melpazoid";
      version = "0";
      src = fetchTarball melpazoidSource.url;
      postUnpack = "mv $sourceRoot/melpazoid/melpazoid.el $sourceRoot";
      meta = {
        inherit (melpazoidSource) description homepage;
        license = lib.licenses.gpl3;
      };
    }
  )
)
++ map (name: epkgs."${name}") (filter (name: elem name [ "package-lint" ]) linters)
