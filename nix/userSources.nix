# This expression returns the same value as user_nix_sources variable
# in the elinter executable script in Nix.
let
  nonEmpty = s: defaultVal: if s == "" then defaultVal else s;
  getEnvDefault = name:
    nonEmpty (builtins.getEnv name);
  userConfigDir =
    getEnvDefault "XDG_CONFIG_HOME"
      ((getEnvDefault "HOME" (throw "HOME cannot be empty")) + "/.config");
  sourcesNixFile = userConfigDir + "/elinter/nix/sources.nix";
in
/. + sourcesNixFile
