{
  # Root directory of packages, i.e. .elinter-cache/pkg
  pkgRoot
, # Target elisp package name(s). Only for linting
  target ? null
, # Abstract/concrete version spec, e.g. 26.1, snapshot, or all
  emacsVersions
, # Command to run on each Emacs version
  commands ? [ ]
, linters ? [ ]
  # Like linters, but used for testing.
, extraPackReqs ? [ ]
  # Used only for testing
, caskFile ? null
  # Used only for testing
, extraBuildInputs ? (_: [ ])
}:
with builtins;
let
  pkgRootAsPath = /. + pkgRoot;

  sources = import ./sources.nix;

  elinterLib = import ./lib.nix;

  emacsForCIPath = ./emacsForCI.nix;

  pkgsForLib = import <nixpkgs> { };

  lib = pkgsForLib.lib;

  lintersAsStrings =
    if builtins.isList linters
    then linters
    else filter isString (builtins.split " " linters);

  # Some linters are shipped with Emacs.
  # Transform to a list of package names.
  linterPackages =
    elinterLib.excludeBuiltinElispPackages lintersAsStrings;

  localPackageNames = getDirectoryDirs pkgRootAsPath;

  excludeFiles = filter (file: match "(.*/)?flycheck_.+\.el" file == null);

  isElisp = file: match ".+\.el" file != null;

  filterAttrs = pred: attrs:
    listToAttrs
      (
        filter (x: pred x.name x.value)
          (map (name: { inherit name; value = attrs.${name}; }) (attrNames attrs))
      );

  getDirectoryFiles = path:
    (attrNames (filterAttrs (_: v: v == "regular" || v == "symlink") (readDir path)));

  getDirectoryDirs = path:
    (attrNames (filterAttrs (_: v: v == "directory") (readDir path)));

  packageAttrsFor = pname: rec {
    src = pkgRootAsPath + "/${pname}";
    files = excludeFiles (getDirectoryFiles src);
    packageFiles = filter isElisp files;
    sourceFiles = filter isElisp packageFiles;
    mainFile = "${pname}.el";
    mainSource = readFile (src + "/${mainFile}");
    minEmacsVersion =
      elinterLib.emacsVersionFromHeader mainSource;
    elispDependencies =
      elinterLib.packageDependenciesFromMainSource mainSource;
  };

  elispExternalDependenciesFor = pname:
    lib.flatten
      (
        map
          (
            dep:
            (
              if elem dep localPackageNames
              then elispExternalDependenciesFor dep
              else [ dep ]
            )
          )
          (packageAttrsFor pname).elispDependencies
      );

  packageAttrs = packageAttrsFor target;

  # Concrete Emacs versions as a list
  concreteEmacsVersions_ = min:
    if emacsVersions == "latest"
    then [ elinterLib.latestStableEmacsVersion ]
    else if emacsVersions == "min"
    then [ min ]
    else if emacsVersions == "all"
    then elinterLib.emacsVersionsSince min
    else [ emacsVersions ];

  concreteEmacsVersions = concreteEmacsVersions_ packageAttrs.minEmacsVersion;

  allPackageAttrs = map packageAttrsFor localPackageNames;

  toBashList = concatStringsSep " ";

  toBashQuotedList = xs: concatStringsSep " "
    (map (s: "'" + s + "'") xs);

  encodeStringList = xs:
    "[" + concatStringsSep " " (map (s: "\"" + s + "\"") xs) + "]";

  localPackageLoadPath = pkgsForLib.lib.concatMapStrings
    (name: "${pkgRootAsPath}/${name}:");

  makeShellAttrsForMultiVersions =
    { availableLocalPackages
    , elispPackages
    , targetEmacsVersions
    , extraBuildInputs ? [ ]
    }: rec {
      EMACSLOADPATH = localPackageLoadPath availableLocalPackages;

      inherit elispPackages;

      buildInputs = [
        sources.ansi
      ]
      ++ (
        map
          (
            version: (
              import emacsForCIPath {
                inherit version elispPackages;
                libNix = ./lib.nix;
              }
            ).package
          )
          targetEmacsVersions
      )
      ++ extraBuildInputs;

      shellHook =
        if length commands != 0
        then ''
          . ${sources.ansi}/ansi
          set +e
          r=0
          for version in ${toBashList targetEmacsVersions}; do
            ansi --yellow "Using $version"
            for command in ${toBashQuotedList commands}; do
              if ! "''${ELINTER_NIX_SHELL}" --argstr version $version \
                        --arg elispPackages '${encodeStringList elispPackages}' \
                        --arg libNix ${./lib.nix} \
                        --run "$command" \
                        -A shell "${emacsForCIPath}"; then
                r=1
              fi
            done
            if [[ $r -gt 0 ]]; then
              # Skip the following versions if any error occurs
              break
            fi
          done
          exit $r
        ''
        else "";
    };

  caskReqs =
    if isString caskFile && caskFile != ""
    then elinterLib.packageDependenciesFromCask (readFile (/. + caskFile))
    else [ ];

in
{

  # Shell for linting and testing
  lint =
    assert (target != null);
    pkgsForLib.mkShell (
      (
        makeShellAttrsForMultiVersions {
          availableLocalPackages = pkgsForLib.lib.intersectLists
            localPackageNames
            packageAttrs.elispDependencies;
          elispPackages =
            lib.unique (linterPackages ++ elispExternalDependenciesFor target);
          targetEmacsVersions = concreteEmacsVersions;
        }
      ) // {
        PACKAGE_NAME = target;
        PACKAGE_ELISP_FILES = concatStringsSep " " (map baseNameOf packageAttrs.sourceFiles);
        PACKAGE_MAIN_FILE = packageAttrs.mainFile;
      }
    );

  test =
    assert (target == null);
    pkgsForLib.mkShell
      (
        makeShellAttrsForMultiVersions {
          availableLocalPackages = localPackageNames;
          elispPackages =
            extraPackReqs ++ caskReqs ++ (
              lib.subtractLists localPackageNames
                (
                  lib.unique (
                    lib.flatten (
                      map (x: x.elispDependencies)
                        allPackageAttrs
                    )
                  )
                )
            );
          targetEmacsVersions =
            concreteEmacsVersions_
              (elinterLib.maxEmacsVersion (map (x: x.minEmacsVersion) allPackageAttrs));
          # Is it better to pass pkgs?
          extraBuildInputs = extraBuildInputs { };
        }
      );

}
