{ lib
, writeText
, linkFarm
, emacsCIVersions
}:
with builtins;
{ minimumEmacsVersion
, lockDirName
, localPackages
, lispFiles
, lispDirs
}:
let
  emacsVersions = lib.pipe (attrNames emacsCIVersions) [
    (map (name: replaceStrings [ "-" ] [ "." ] (lib.removePrefix "emacs-" name)))
    (filter (version:
      version == "snapshot"
      || compareVersions version minimumEmacsVersion >= 0))
  ];

  trimLeft = text: head (match "[[:space:]]*(.+)" text);

  trimRight = text:
    if match "(.+[^[:space:]])[[:space:]]*" text != null
    then head (match "(.+[^[:space:]])[[:space:]]*" text)
    else text;

  trim = text: trimRight (trimLeft text);

  emacsArgs =
    "-l package --eval \"(package-initialize)\" ${lib.concatMapStringsSep " " (s: "-L " + s) lispDirs}";

  # HACK
  prependEmacsArgs = cmdline:
    if match "emacs([[:space:]].+)" cmdline != null
    then "emacs " + emacsArgs + head (filter isString (match "emacs([[:space:]].+)" cmdline))
    else cmdline;

  makeWorkflowBody = name:
    { text
    , compile ? false
    , github ? { }
    , description ? null
    , ...
    }: ''
      name: '${name}'
      on: ${github.on or "push: { paths: ['**.el'] }"}
      jobs:
        ${name}:
          runs-on: ubuntu-latest
          strategy:
            matrix:
              emacs_version: ${toJSON emacsVersions}
          steps:
          - uses: purcell/setup-emacs@master
            with:
              version: ''${{ matrix.emacs_version }}
          - uses: actions/checkout@v2
          - name: Install dependencies
            run: |
              cat <(jq -r '.nodes.root.inputs | map(.) | .[]' ${lockDirName}/flake.lock) \
                  <(jq -r 'keys | .[]' ${lockDirName}/archive.lock) \
                  | xargs emacs -batch -l package --eval \
                  "(progn
                      (push '(\"melpa\" . \"https://melpa.org/packages/\")
                            package-archives)
                      (package-initialize)
                      (when command-line-args-left
                        (package-refresh-contents))
                      (dolist (package-name command-line-args-left)
                        (let ((package (intern package-name)))
                           (when (and package
                                      (not (memq package
                                                 '(${concatStringsSep " " localPackages}))))
                             (package-install (cadr (assq package 
                                                          package-archive-contents)))))))"
          - name: Byte-compile
            run: |
              emacs -batch -l bytecomp ${emacsArgs} \
                --eval "(setq byte-compile-error-on-warn t)" \
                -f batch-byte-compile ${lib.escapeShellArgs lispFiles}
          - run: |
              ${prependEmacsArgs (trim text)}
            ${lib.optionalString (description != null) "name: ${description}"}
    '';
in
scripts:
linkFarm "github-workflows"
  (lib.mapAttrsToList
    (name: options: {
      name = "${name}.yml";
      path = writeText "github-workflow-${name}" (makeWorkflowBody name options);
    })
    scripts)
