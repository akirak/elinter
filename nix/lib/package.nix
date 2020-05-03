let pkgs = import ../pkgs.nix;
in with pkgs.lib;
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

  verifyDhallPackageList = emacsDerivation: srcDir: packageFile:
    pkgs.stdenv.mkDerivation {
      name = package.pname + "-package-lint";
      buildInputs = [
        (emacsWithPackages emacsDerivation (epkgs:
          (package.dependencies epkgs) ++ [ epkgs.melpaPackages.package-lint ]))
      ];
      shellHook = let
        # Assume the items of files never contain space
        localDeps = pkgs.lib.concatMapStringsSep " " (pkg: pkg.pname)
          (package.localDependencies or [ ]);
        mainFile =
          # package.mainFile can be null if the package is converted
          # from Dhall, so the null check is necessary.
          if package ? mainFile && !(isNull package.mainFile) then
            package.mainFile
          else
            "";
      in ''
        echo
        echo ==========================================================
        echo package-lint on ${package.pname} package
        echo ==========================================================
        cd ${package.src}
        emacs --no-site-file --batch \
           --eval "(setq explicitly-installed-packages '(${localDeps}))" \
           --eval "(setq package-lint-main-file \"${mainFile}\")" \
           -l ${./package-lint-runner.el} ${concatShArgs package.files}
        result=$?
        echo ----------------------------------------------------------
        if [[ $result -eq 0 ]]; then
          echo "No package-lint errors found."
        else
          echo "Errors found by package-lint."
        fi
        # Prevent from actually entering the shell
        exit $result
      '';
    };

}
