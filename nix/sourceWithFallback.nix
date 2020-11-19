# Retrieve a source of a particular package from the user's
# sources.json.
#
# If the name is unavailable, a corresponding source in this
# repository is used as a fallback.
#
# - `sources` should be a path to sources.nix.
# - `name` should be the name in sources.json
sources: name:
if sources != null
&& builtins.pathExists sources
&& builtins.hasAttr name (import sources)
then (import sources).${name}
else (import ./sources.nix).${name}
