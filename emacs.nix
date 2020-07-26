{ pkgs ? import <nixpkgs> {
    overlays = [
      (import (import ./nix/sources.nix).emacs-overlay)
    ];
  }
, # Main file of the package, given as an absolute path string
  mainFile ? null
, emacs ? "emacs"
}:
with builtins;
with pkgs;
let
  gitignore = import (import ./nix/sources.nix).gitignore { inherit (pkgs) lib; };

  emacsForCI = emacsWithPackagesFromPackageRequires {
    package = pkgs."${emacs}";
    packageElisp = readFile (/. + mainFile);
    extraEmacsPackages = epkgs: with epkgs; [
      package-lint
    ];
  };

in
{
  # Shell for linting and testin
  development = pkgs.mkShell {
    buildInputs = [
      emacsForCI
    ];
  };

  compile =
    let
      pname = getEnv "PACKAGE_NAME";
      pwd = getEnv "PWD";
      sourceFilesInString = getEnv "PACKAGE_ELISP_FILES";
    in
      stdenv.mkDerivation {
        name = "elinter-${emacs}-package-${pname}";
        version = "0";

        src = gitignore.gitignoreSource (/. + pwd);

        buildInputs = [ emacsForCI ];

        phases = [ "buildPhase" "installPhase" ];

        buildPhase = ''
          result=0

          ls -l $src

          for f in ${sourceFilesInString}; do
            cp $src/$f .
            emacs --batch --no-site-file --eval "(setq byte-compile-error-on-warn t)" \
              --funcall batch-byte-compile $f
            if [[ $? -gt 0 ]]; then
              result=1
            fi
          done

          exit $result
        '';

        installPhase = ''
          mkdir $out
          cp -r $src/*.* $out
          cp -r *.* $out
          emacs --batch -Q --eval \
             "(with-temp-buffer
                (insert emacs-major-version)
                 (write-region (point-min) (point-max) \"$out/.elinter-emacs-version\"))"
        '';
      };

}
