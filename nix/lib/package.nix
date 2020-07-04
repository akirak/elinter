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
        let
          packageListFromNames = names: epkgs:
            forEach names (depName:
              if elem depName localPackages then
                localMelpaBuild epkgs self."${depName}"
              else
                epkgs."${depName}");
        in listToAttrs (forEach plainPackageList (x: {
          name = x.pname;
          value = x // {
            src = srcDir;
            recipe = pkgs.writeText "recipe" x.recipe;
            dependencies = packageListFromNames (x.dependencies or [ ]);
            testDependencies = packageListFromNames (x.testDependencies or [ ]);
            localDependencies =
              forEach x.localDependencies (depName: self."${depName}");
            # Only used for information to the user
            dependencyNames = x.dependencies or [ ];
            testDependencyNames = x.testDependencies or [ ];
            localDependencyNames = x.localDependencies or [ ];
          };
        }));
    in fix f;

}
