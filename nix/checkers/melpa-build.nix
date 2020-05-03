{ pkgs, emacsDerivation }:
package:
with (import ../lib);
   pkgs.emacsPackages.melpaBuild {
        inherit (package) pname version src files recipe;
        packageRequires = package.dependencies pkgs.emacsPackages;
      }
