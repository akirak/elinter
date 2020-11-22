# Functions in this file are basically private and intended for use
# from other Nix sources inside this repository.
#
# There can be deletion from the exposed attribute sets, so don't use
# them directly from shell scripts or other repositories.
with builtins;
let
  sources = import ./sources.nix;

  pkgsForLib = import <nixpkgs> {};

  lib = pkgsForLib.lib;

  emacsOverlayParseLib =
    pkgsForLib.callPackage (sources.emacs-overlay + "/parse.nix") {};

  elispHelperLib = import sources.nix-elisp-helpers {};

  nonEmpty = s: defaultVal: if s == "" then defaultVal else s;

  # Get the value of an environment variable
  getEnvDefault = name:
    nonEmpty (builtins.getEnv name);

  # Get the value of a platform-independent user configuration directory
  userConfigDir =
    getEnvDefault "XDG_CONFIG_HOME"
      ((getEnvDefault "HOME" (throw "HOME cannot be empty")) + "/.config");

  # This expression returns the same value as user_nix_sources variable
  # in the elinter executable script in Nix.
  userSources =
    /. + (userConfigDir + "/elinter/nix/sources.nix");

  packageDependenciesFromMainSource =
    emacsOverlayParseLib.parsePackagesFromPackageRequires;

  packageDependenciesFromCask = str:
    let
      cask = elispHelperLib.parseCask str;
      depsWithMaybeVersions =
        (cask.dependencies or []) ++ ((cask.development or {}).dependencies or []);
    in
      map head depsWithMaybeVersions;

  splitQuotedString = str:
    let
      m = match "'([^']+)'( +(.+))?" str;
      first = elemAt m 0;
      rest = elemAt m 2;
    in
      if m == null
      then []
      else [ first ] ++ (
        if rest == null
        then []
        else splitQuotedString rest
      );

  # Retrieve a source of a particular package from the user's
  # sources.json.
  #
  # If the name is unavailable, a corresponding source in this
  # repository is used as a fallback.
  #
  # - `name` should be the name in sources.json
  sourceWithFallback = name:
    if userSources != null
    && pathExists userSources
    && hasAttr name (import userSources)
    then (import userSources).${name}
    else sources.${name};

  # nixpkgs with emacs-overlay
  pkgsWithEmacsOverlay =
    import <nixpkgs> {
      overlays = [
        (import (sourceWithFallback "emacs-overlay"))
      ];
    };

  emacs-ci = import (sourceWithFallback "nix-emacs-ci");

  emacsPackageFromName = emacsName:
    if match "emacs-.+" emacsName != null
    then emacs-ci."${emacsName}"
    else pkgsWithEmacsOverlay."${emacsName}";

  localEmacsPackagesFor = emacs:
    (pkgsWithEmacsOverlay.emacsPackagesFor (emacsPackageFromName emacs)).overrideScope'
      (
        _: esuper: {
          melpazoid = esuper.trivialBuild {
            pname = "melpazoid";
            version = "0";
            src = fetchTarball (sourceWithFallback "melpazoid").url;
            postUnpack = "mv $sourceRoot/melpazoid/melpazoid.el $sourceRoot";
          };
        }
      );

  emacsDerivation =
    { emacs ? "emacs"
    , # elisp packages as a list of strings
      dependencies ? []
    }:
      (localEmacsPackagesFor emacs).emacsWithPackages (
        epkgs:
          map (name: epkgs.${name}) dependencies
      );

  attrNameToVersion = name:
    lib.replaceStrings [ "-" ] [ "." ]
      (lib.removePrefix "emacs-" name);

  # Retrieve a sorted list of Emacs versions available, including
  # "snapshot".
  compareEmacsVersions = v1: v2:
    if v1 == "snapshot" then
      true
    else if v2 == "snapshot" then
      false
    else
      compareVersions v1 v2 >= 0;

  # List of Emacs versions available from nix-emacs-ci, in descending
  # order
  descendingEmacsVersions = sort compareEmacsVersions
    (map attrNameToVersion (attrNames emacs-ci));

  # The latest stable version is the first entry excluding snapshot.
  latestStableEmacsVersion = head (filter (v: v != "snapshot") descendingEmacsVersions);

  emacsVersionsSince = minVersion:
    filter (v: compareEmacsVersions v minVersion) descendingEmacsVersions;

  # Find a Package-Requires library header line
  #
  # This is used for determining the minimum Emacs Version of the
  # package.
  #
  # Since only the first line is matched, the Emacs dependency should
  # appear at the beginning.
  isPackageLine = s: isString s && match ";;+ *Package-Requires: .+" s != null;

  # Retrieve the minimum version from the header.
  emacsVersionFromHeader = str:
    let
      packageRequiresHeaderLines = filter isPackageLine (split "\n" str);
      header =
        assert (length packageRequiresHeaderLines > 0);
        head packageRequiresHeaderLines;
      versions =
        assert (header != null);
        builtins.match ".+\\(emacs \"([.[:digit:]]+)\"\\).+" header;
    in
      assert (length versions > 0);
      head versions;

  builtinPackages = [ "checkdoc" "check-declare" ];

  excludeBuiltinElispPackages =
    filter (name: ! (elem name builtinPackages));

in
{
  inherit packageDependenciesFromCask;
  inherit splitQuotedString;
  inherit packageDependenciesFromMainSource;
  inherit emacsDerivation;
  inherit emacsVersionFromHeader;
  inherit latestStableEmacsVersion;
  inherit descendingEmacsVersions;
  inherit emacsVersionsSince;
  inherit excludeBuiltinElispPackages;
}
