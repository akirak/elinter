{ pkgs ? import <nixpkgs> {}
  # Whether to turn on experimental checks using melpazoid.
, useMelpazoid ? false
}:
with pkgs;
with (import (import ./nix/sources.nix).gitignore { inherit (pkgs) lib; });
let
  elinterLib = import ./nix/lib.nix;

  linters = [
    "checkdoc"
    "check-declare"
    "package-lint"
  ] ++ (if useMelpazoid then [ "melpazoid" ] else []);

  # A list of linter names joined by space.
  # This should be consistent with ELINTER_LINTERS environment variables
  # in elinter-run-linters script.
  lintersAsString = lib.concatStringsSep " " linters;

  src = gitignoreSource ./.;

  share = runCommandNoCC "elinter-share"
    {
      inherit src;
      preferLocalBuild = true;
    }
    ''
      mkdir -p $out/share/elinter
      cp -r -t $out/share/elinter $src/share/*.* $src/nix

      substituteInPlace $out/share/elinter/nix/lib.nix \
        --replace './sources.nix' "$out/share/elinter/nix/sources.nix"
    '';

  # Alternative interface which receives files as arguments and run
  # static linting on them.
  #
  # This can be used to implement checks in the Git pre-commit hook.
  file-linter =
    let
      emacsForLint = (
        import ./nix/emacsForCI.nix {
          version = elinterLib.latestStableEmacsVersion;
          elispPackages =
            elinterLib.excludeBuiltinElispPackages linters
            ++ [ "package-build" ];
          libNix = ./nix/lib.nix;
        }
      ).package;
    in
      runCommandNoCC "elinter-file-linter" {
        preferLocalBuild = true;

        propagateBuildInputs = [
          emacsForLint
          share
        ];

        buildInputs = [
          makeWrapper
        ];
      }
        ''
          mkdir -p $out/bin
          makeWrapper ${emacsForLint}/bin/emacs $out/bin/elinter-lint-files \
            --run "exec &> >(${gnugrep}/bin/grep -E -f ${share}/share/elinter/file-linter-patterns.txt)" \
            --add-flags "-Q --batch" \
            --add-flags "--script ${share}/share/elinter/elinter-run-linters.el" \
            --set-default ELINTER_LINTERS "${lintersAsString}" \
            --set-default ELINTER_LINT_CUSTOM_FILE ${share}/share/elinter/lint-options.el
        '';

  runners = runCommandNoCC "elinter-linters" {
    preferLocalBuild = true;
    inherit src;
    buildInputs = [
      makeWrapper
    ];
    propagateBuildInputs = [
      share
    ];
  }
    ''
      mkdir -p $out/bin
      for f in elinter-logger elinter-github-logger \
          elinter-byte-compile elinter-run-linters; do
        cp $src/bin/$f $out/bin
        chmod +x $out/bin/$f
      done
      substituteInPlace $out/bin/elinter-run-linters \
        --replace share/workflow.bash ${share}/share/elinter/workflow.bash \
        --replace share/lint-options.el ${share}/share/elinter/lint-options.el \
        --replace share/elinter-run-linters.el ${share}/share/elinter/elinter-run-linters.el
      substituteInPlace $out/bin/elinter-byte-compile \
        --replace share/workflow.bash ${share}/share/elinter/workflow.bash
      substituteInPlace $out/bin/elinter-github-logger \
        --replace share/github-log.sed ${share}/share/elinter/github-log.sed

      # Patch backend scripts to redirect output to the logger
      for backend in elinter-run-linters elinter-byte-compile; do
        wrapProgram $out/bin/$backend --run "exec &> >(elinter-logger)"
      done
    '';

  main = symlinkJoin rec {
    name = "elinter";
    version = "0";
    preferLocalBuild = true;
    paths = [
      share
    ];
    propagateBuildInputs = [
      file-linter
      runners
    ];
    buildInputs = [
      makeWrapper
    ];
    postBuild = ''
      mkdir -p $out/share/elinter

      unlink $out/share/elinter/pre-commit-config.yaml
      cp ${share}/share/elinter/pre-commit-config.yaml $out/share/elinter/.pre-commit-config.yaml

      substituteInPlace $out/share/elinter/.pre-commit-config.yaml \
        --replace "'elinter-lint-files'" ${file-linter}/bin/elinter-lint-files

      cp "${fetchTarball (import ./nix/sources.nix).ansi.url}/ansi" $out/share/elinter

      mkdir $out/bin
      cp ${src}/bin/elinter $out/bin

      # Substitute paths to the library source files.
      lib=$out/share/elinter

      substituteInPlace $out/bin/elinter \
        --replace '${lintersAsString}' "${lintersAsString}" \
        --replace "share/.pre-commit-config.yaml" $lib/.pre-commit-config.yaml \
        --replace "share/ansi" "$lib/ansi" \
        --replace "share/workflow.bash" "$lib/workflow.bash" \
        --replace "share/nix/" "$lib/nix/"

      wrapProgram $out/bin/elinter \
        --argv0 elinter \
        --prefix PATH : "${runners}/bin" \
        --set-default ELINTER_LINTERS "${lintersAsString}" \
        --set ELINTER_VERSION $version
    '';
  };

in
{
  inherit file-linter;
}
// main
