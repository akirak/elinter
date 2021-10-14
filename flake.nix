{
  description = "An Emacs Lisp lint runner";

  inputs.flake-utils.url = "github:numtide/flake-utils";
  inputs.pre-commit-hooks = {
    url = "github:cachix/pre-commit-hooks.nix";
    inputs.nixpkgs.follows = "nixpkgs";
    inputs.flake-utils.follows = "flake-utils";
  };
  inputs.gitignore = {
    url = "github:hercules-ci/gitignore.nix";
    inputs.nixpkgs.follows = "nixpkgs";
  };
  inputs.emacs-overlay = {
    url = "github:nix-community/emacs-overlay";
    inputs.nixpkgs.follows = "nixpkgs";
  };

  outputs = { self, nixpkgs, flake-utils, gitignore, pre-commit-hooks, emacs-overlay }:
    flake-utils.lib.eachSystem
      [
        "x86_64-linux"
        "x86_64-darwin"
        "aarch64-linux"
        "i686-linux"
      ]
      (
        system:
        let
          pkgs = import nixpkgs {
            inherit system;
            overlays = [
              emacs-overlay.overlay
            ];
          };
          inherit (gitignore.lib) gitignoreSource;
          # fromElisp = (import inputs.fromElisp) { inherit pkgs; };
          lispSrc = gitignoreSource ./lisp;
          elispLinter = pkgs.callPackage ./lint.nix {
            src = lispSrc;
          };
        in
        {
          packages = flake-utils.lib.flattenTree {
            elinter-lint = elispLinter {
              recipeDir = ./.recipes;
              package = pkgs.emacs;
            };
            elinter-compile = pkgs.runCommandNoCC "elinter-compile"
              {
                src = gitignoreSource ./bash;
                propagatedBuildInputs = [
                  lispSrc
                ];
              } ''
              mkdir -p $out/bin
              cp $src/elinter-compile.bash $out/bin/elinter-compile
              substituteInPlace $out/bin/elinter-compile \
                --replace elinter-install-deps.el "${lispSrc}/elinter-install-deps.el"
            '';
          };
          checks = {
            pre-commit-check = pre-commit-hooks.lib.${system}.run {
              src = gitignoreSource ./.;
              hooks = {
                nixpkgs-fmt.enable = true;
                nix-linter.enable = true;
                elinter = {
                  enable = true;
                  name = "elinter";
                  description = "Lint Emacs Lisp files";
                  entry = "${self.packages.${system}.elinter-lint}/bin/elinter-lint";
                  files = "\\.el$";
                  excludes = [
                    "\\.dir-locals\\.el"
                  ];
                };
              };
            };
          };
          devShell = nixpkgs.legacyPackages.${system}.mkShell {
            buildInputs = [
              self.packages.${system}.elinter-lint
              self.packages.${system}.elinter-compile
            ];

            inherit (self.checks.${system}.pre-commit-check) shellHook;
          };
        }
      );
}
