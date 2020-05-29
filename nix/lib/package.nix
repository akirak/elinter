{ pkgs }:
with pkgs.lib;
with builtins; {
  # Build an internal representation for package list from
  # parsed dhall package data
  parsePackageList = srcDir: plainPackageList:
    let
      localPackages = map (p: p.pname) plainPackageList;
      localMelpaBuild = epkgs: pkg:
        epkgs.melpaBuild {
          inherit (pkg) pname version src files recipe;
          packageRequires = pkg.dependencies epkgs;
        };
      f = self:
        listToAttrs (forEach plainPackageList (x: {
          name = x.pname;
          value = x // {
            src = srcDir;
            recipe = pkgs.writeText "recipe" x.recipe;
            dependencies = epkgs:
              forEach x.dependencies (depName:
                if elem depName localPackages then
                  localMelpaBuild epkgs self."${depName}"
                else
                  epkgs.melpaPackages."${depName}");
            localDependencies =
              forEach x.localDependencies (depName: self."${depName}");
            # Only used for information to the user
            dependencyNames = x.dependencies;
          };
        }));
    in fix f;

}
