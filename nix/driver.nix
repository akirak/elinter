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
      linterPackages
      ++ pkgsForLib.lib.subtractLists localPackageNames packageAttrs.elispDependencies;

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
 
      if [[ -v ELINTER_BEFORE_PACKAGE_HOOK && -n "''${ELINTER_BEFORE_PACKAGE_HOOK}" ]]; then
        ''${ELINTER_BEFORE_PACKAGE_HOOK}
      fi

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
      set -e

      if [[ $r -eq 0 ]]; then
        if [[ -v ELINTER_PACKAGE_SUCCESS_HOOK && -n "''${ELINTER_PACKAGE_SUCCESS_HOOK}" ]]; then
          ''${ELINTER_PACKAGE_SUCCESS_HOOK}
        fi
      else
        ansi --red "Package ${target} failed on some checks."
        if [[ -v ELINTER_PACKAGE_FAILURE_HOOK && -n "''${ELINTER_PACKAGE_FAILURE_HOOK}" ]]; then
          ''${ELINTER_PACKAGE_FAILURE_HOOK}
        fi
      fi

      if [[ -v ELINTER_AFTER_PACKAGE_HOOK && -n "''${ELINTER_AFTER_PACKAGE_HOOK}" ]]; then
        # shellcheck disable=SC2090
        ''${ELINTER_AFTER_PACKAGE_HOOK}
      fi

      exit $r
    '';
  };

}
