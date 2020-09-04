{ pkgs ? import <nixpkgs> {} }:
with pkgs;
let
  src = stdenv.mkDerivation {
    name = "bad-hello";
    version = "0";
    src = ./.;

    phases = [ "buildPhase" "installPhase" ];

    buildInputs = [ coreutils ];

    buildPhase = ''
      cat > bad-hello.el <<SRC
      (defun bad-hello-message ()
              (message "Hello"))
      SRC

      mkdir .recipes
      cat > .recipes/bad-hello <<SRC
      (bad-hello :fetcher github :repo "akirak/elinter"
                       :files ("bad-hello.el"))
      SRC
    '';

    installPhase = ''
      mkdir $out
      cp -rt $out .recipes *.el
    '';
  };
in
''
  cd ${src}
  ! elinter
''
