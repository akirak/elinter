{ writeShellApplication
, emacsWithElsa
}:
writeShellApplication {
  name = "elsa";
  runtimeInputs = [
    emacsWithElsa
  ];

  text = ''
    exec emacs -batch -q -l elsa -f elsa-run-files-and-exit "$@"
  '';
}
