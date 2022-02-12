{
  description = "Your greatest Emacs Lisp package ever";

  inputs = {
    gnu-elpa = {
      url = "git+https://git.savannah.gnu.org/git/emacs/elpa.git?ref=main";
      flake = false;
    };
    melpa = {
      # FIXME: Fork MELPA, create a new branch for your package, and push it to GitHub
      url = "github:OWNER/melpa/BRANCH";
      # TODO: After your package is on MELPA, switch to master.
      # url = "github:melpa/melpa";
      flake = false;
    };

    elinter = {
      url = "github:akirak/elinter/v5";
      inputs.gnu-elpa.follows = "gnu-elpa";
      inputs.melpa.follows = "melpa";
    };
  };

  outputs =
    { self
    , elinter
    , ...
    } @ inputs:
    elinter.lib.mkFlake {
      src = ./.;
      lockDirName = "lock";
      localPackages = [
        (builtins.throw "FIXME: Put your package name here.")
      ];
    };
}
