{ pkgs ? import <nixpkgs> { } }: rec {

  hello = {
    pname = "hello";
    version = "0.1";
    src = ../.;
    files = [ "tests/hello.el" "tests/hello-util.el" ];
    dependencies = epkgs: (with epkgs.melpaPackages; [ dash ]);
    mainFile = "tests/hello.el";
    recipe = pkgs.writeText "recipe" ''
      (hello :fetcher github :repo "akirak/emacs-package-checker"
             :files ("tests/hello.el" "tests/hello-util.el"))
    '';
  };

  hello2 = {
    pname = "hello2";
    version = "0.1";
    src = ../.;
    files = [ "tests/hello2.el" ];
    localDependencies = [ hello ];
    dependencies = epkgs:
      [
        (epkgs.melpaBuild {
          inherit (hello) pname version src files recipe;
          packageRequires = hello.dependencies epkgs;
        })
      ];
    recipe = pkgs.writeText "recipe" ''
      (hello2 :fetcher github :repo "akirak/emacs-package-checker"
             :files ("tests/hello2.el"))
    '';
  };

}
