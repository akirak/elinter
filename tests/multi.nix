{ pkgs ? import <nixpkgs> {},
  emacs ? import ./emacs.nix
}:
let
  melpaCheck = import ../.;
  hello = melpaCheck {
    inherit emacs pkgs;
    pname = "hello";
    version = "0.1";
    src = ../.;
    files = ["tests/hello.el"];
    dependencies = epkgs: (with epkgs.melpaStablePackages; [
      dash
    ]);
    recipe = pkgs.writeText "recipe" ''
(hello :fetcher github :repo "akirak/emacs-package-checker"
       :files ("tests/hello.el"))
'';
  };
  hello2 = melpaCheck {
    inherit emacs pkgs;
    pname = "hello";
    version = "0.1";
    src = ../.;
    files = ["tests/hello.el"];
    dependencies = epkgs: (with epkgs.melpaStablePackages; [
      hello.melpaBuild
    ]);
    recipe = pkgs.writeText "recipe" ''
(hello :fetcher github :repo "akirak/emacs-package-checker"
       :files ("tests/hello.el"))
'';
  };
in {
  checkdoc = pkgs.stdenv.mkDerivation {
    name = "checkdoc-mypackage";
    buildInputs = [
      hello.checkdoc
      hello2.checkdoc
    ];
    shellHook = ''echo OK'';
  };
}
