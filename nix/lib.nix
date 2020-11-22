# Functions in this file are basically private and intended for use
# from other Nix sources inside this repository.
#
# There can be deletion from the exposed attribute sets, so don't use
# them directly from shell scripts or other repositories.
with builtins;
let
  sources = import ./sources.nix;

  emacsOverlayParseLib =
    pkgs.callPackage (sources.emacs-overlay + "/parse.nix") {};

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

in
{
  inherit packageDependenciesFromCask;
  inherit splitQuotedString;
  inherit packageDependenciesFromMainSource;
  inherit emacsDerivation;
}
