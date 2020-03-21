{ pkgs ? import <nixpkgs> {},
  system ? builtins.currentSystem,
  pname ? "awesome-emacs-package",
  version,
  recipe,
  emacs ? pkgs.emacs,
  dependencies, src, files,
  elpaCache ? "/dev/shm/package-lint/elpa/${pname}"
}:
let
  # Emacs with packages specified as dependencies from outside of this
  # nix file. This is used for byte-compiling the package.
  emacsWithPackages = (pkgs.emacsPackagesNgGen emacs).emacsWithPackages
    dependencies;
  # Emacs with package-lint. This is used for running package-lint.
  emacsForPackageLint = (pkgs.emacsPackagesNgGen emacs).emacsWithPackages
    (epkgs: (with epkgs.melpaPackages; [ package-lint ]));
in rec
{

  # Deprecated. Use melpaBuild instead.
  byte-compile = derivation {
    inherit src files system;
    name = pname + "-byte-compile";
    builder = "${pkgs.bash}/bin/bash";
    args = [ ./byte-compile.sh ];
    buildInputs = [ pkgs.coreutils emacsWithPackages ];
  };

  checkdoc =
    let
      script = pkgs.writeScript "checkdoc.sh" ''
#!${pkgs.bash}/bin/bash

# Run checkdoc on target files

# This script should be run by the builder in a nix-build task
# defined in default.nix.

# The logic is based on an implementation in makel:
# <https://gitlab.petton.fr/DamienCassou/makel/blob/master/makel.mk>

# Fail if any error occurs inside this script
set -e

# Set PATH from buildInputs
unset PATH
for p in $buildInputs; do
    export PATH=$p/bin${PATH:+:}$PATH
done

if [ -n "$src" ]; then
    cd $src
fi

echo "Running checkdoc..."

# nix-build fails if you don't make the output directory
mkdir -p $out

# Create a log file in the output directory
logFile=$out/checkdoc.log
touch $logFile

for f in $files; do
    # Run checkdoc on each file and append the output to the log file
    emacs --batch --eval "(checkdoc-file \"$f\")" 2>&1 | tee -a $logFile
done

# Tell the location of the log file to the user
echo "Saved the checkdoc status to $logFile"

# Check if the log file size is 0, which means no output from the
# program
if [ $(stat --printf='%s' $logFile) -eq 0 ]; then
    echo "The checkdoc result is clean."
else
    # Return non-zero exit code
    echo "The checkdoc result is NOT clean." >&2
    exit 1
fi
'';
    in
      derivation {
        inherit src files system;
        name = pname + "-checkdoc";
        builder = script;
        buildInputs = [ pkgs.coreutils emacs ];
      };

  # Since package-lint requires the internet connection to test
  # if dependencies are installable, you can only run this command
  # in nix-shell, and not in nix-build.
  package-lint = pkgs.stdenv.mkDerivation {
    name = pname + "-package-lint";
    buildInputs = [ emacsForPackageLint ];
    shellHook =
    let
    # Assume the items of files never contain space
      args = pkgs.lib.foldr (a: b: a + " " + b) "" files;
    in ''
    set -e
    cd ${src}
    ELPA_USER_DIR=${elpaCache} emacs --no-site-file --batch -l ${./run-package-lint.el} ${args}
    echo "package-lint is OK."
    # Prevent from actually entering the shell
    exit
    '';
  };

  buttercup =
    let
      emacsWithButtercup = (pkgs.emacsPackagesNgGen emacs).emacsWithPackages
        (epkgs:
          [epkgs.melpaPackages.buttercup]
          # ++ dependencies epkgs
          ++ [melpaBuild]);
    in
      pkgs.stdenv.mkDerivation {
        name = pname + "-buttercup";
        buildInputs = [ emacsWithButtercup ];
        shellHook =
      ''
      echo "Running buttercup..."
      set -e
      out=$(mktemp)
      cd ${src}
      emacs --batch --no-site-file \
          --load package --eval '(setq package-archives nil)' \
          -f package-initialize \
          --load buttercup -f buttercup-run-discover
      exit
      '';
      };

  melpaBuild =
    pkgs.emacsPackages.melpaBuild {
      inherit pname version src files recipe;
      packageRequires = dependencies pkgs.emacsPackages;
    };

}
