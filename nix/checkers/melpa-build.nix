{ pkgs, customEmacsPackages, ... }:
package:
with (import ../lib { inherit pkgs; });
pkgs.emacsPackages.melpaBuild {
  inherit (package) pname version src files recipe;
  packageRequires = package.dependencies pkgs.emacsPackages;
}
