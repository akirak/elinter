sources: name:
if sources != null && builtins.pathExists sources && builtins.hasAttr name (import sources)
then (import sources).${name}
else (import ./sources.nix).${name}
