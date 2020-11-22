{
  # Root directory of packages, i.e. .elinter-cache/pkg
  pkgRoot
, # Target elisp package name(s)
  target
, # Abstract/concrete version spec, e.g. 26.1, snapshot, or all
  emacsVersions
, # Command to run on each Emacs version
  command ? ""
, linters ? []
}:
with builtins;
let
  pkgRootAsPath = /. + pkgRoot;

  sources = import ./sources.nix;

  elinterLib = import ./lib.nix;

  emacsForCIPath = ./emacsForCI.nix;

  pkgsForLib = import <nixpkgs> {};

  lib = pkgsForLib.lib;

  lintersAsStrings =
    if builtins.isList linters
    then linters
    else filter isString (builtins.split " " linters);

  builtinPackages = [ "checkdoc" "check-declare" ];

  # Some linters are shipped with Emacs.
  # Transform to a list of package names.
  linterPackages =
    filter (name: ! (elem name builtinPackages))
      lintersAsStrings;

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
    src = (assert (isPath pkgRootAsPath); pkgRootAsPath) + "/${pname}";
    files = excludeFiles (getDirectoryFiles src);
    packageFiles = filter isElisp files;
    sourceFiles = filter isElisp packageFiles;
    mainFile = src + "/${pname}.el";
    mainSource = readFile mainFile;
    minEmacsVersion =
      elinterLib.emacsVersionFromHeader mainSource;
    elispDependencies =
      elinterLib.packageDependenciesFromMainSource mainSource;
  };

  elispExternalDependenciesFor = pname:
    lib.flatten
      (
        map (
          dep:
            (
              if elem dep localPackageNames
              then elispExternalDependenciesFor dep
              else [ dep ]
            )
        ) (packageAttrsFor pname).elispDependencies
      );

  packageAttrs = packageAttrsFor target;

  # Concrete Emacs versions as a list
  concreteEmacsVersions =
    if emacsVersions == "latest"
    then [ elinterLib.latestStableEmacsVersion ]
    else if emacsVersions == "min"
    then [ packageAttrs.minEmacsVersion ]
    else if emacsVersions == "all"
    then elinterLib.emacsVersionsSince packageAttrs.minEmacsVersion
    else [ emacsVersions ];

  toBashList = concatStringsSep " ";

  encodeStringList = xs:
    "[" + concatStringsSep " " (map (s: "\"" + s + "\"") xs) + "]";

in
{

  # Shell for linting and testing
  lint = pkgsForLib.mkShell rec {
    EMACSLOADPATH = pkgsForLib.lib.concatMapStrings
      (name: "${pkgRootAsPath}/${name}:")
      (
        pkgsForLib.lib.intersectLists
          localPackageNames
          packageAttrs.elispDependencies
      );

    PACKAGE_NAME = target;
    PACKAGE_ELISP_FILES = concatStringsSep " " (map baseNameOf packageAttrs.sourceFiles);
    PACKAGE_MAIN_FILE = /. + packageAttrs.mainFile;

    elispPackages =
      lib.unique (linterPackages ++ elispExternalDependenciesFor target);

    buildInputs = [
      sources.ansi
    ]
    ++ map
      (
        version: (
          import emacsForCIPath {
            inherit version elispPackages;
            libNix = ./lib.nix;
          }
        ).package
      )
      concreteEmacsVersions;

    shellHook = ''
      . ${sources.ansi}/ansi
 
      set +e
      r=0
      for version in ${toBashList concreteEmacsVersions}; do
        if [[ -n "${command}" ]]; then
          ansi --yellow "Using $version"
          if ! "''${ELINTER_NIX_SHELL}" --argstr version $version \
                   --arg elispPackages '${encodeStringList elispPackages}' \
                   --arg libNix ${./lib.nix} \
                   --run "${command}" \
                   -A shell "${emacsForCIPath}"; then
            r=1
            # Skip the following versions if any error occurs
            break
          fi
          echo
        fi
      done

      exit $r
    '';
  };

}
