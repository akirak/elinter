{ elinterEnv
, elinter-file-linter
, elinter-linters
, makeWrapper
, symlinkJoin
}:
with elinterEnv;
symlinkJoin rec {
  name = "elinter";
  version = "0";
  preferLocalBuild = true;
  paths = [
    share
  ];
  propagateBuildInputs = [
    elinter-file-linter
    elinter-linters
  ];
  buildInputs = [
    makeWrapper
  ];
  postBuild = ''
    mkdir -p $out/share/elinter

    unlink $out/share/elinter/pre-commit-config.yaml
    cp ${share}/share/elinter/pre-commit-config.yaml $out/share/elinter/.pre-commit-config.yaml

    substituteInPlace $out/share/elinter/.pre-commit-config.yaml \
      --replace "'elinter-lint-files'" ${elinter-file-linter}/bin/elinter-lint-files

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
      --prefix PATH : "${elinter-linters}/bin" \
      --set-default ELINTER_LINTERS "${lintersAsString}" \
      --set ELINTER_VERSION $version
  '';
}
