let Package = (../../../schema.dhall).Package

in  [ Package::{
      , pname = "package-lint-runner"
      , version = "0.1"
      , emacsVersion = "25.1"
      , files = [ "nix/checkers/package-lint-runner.el" ]
      , dependencies = [ "package-lint" ]
      , recipe =
          ''
          (package-lint-runner :fetcher github :repo "akirak/melpa-check" :files ("nix/checkers/package-lint-runner.el"))
          ''
      }
    , Package::{
      , pname = "checkdoc-runner"
      , version = "0.1"
      , emacsVersion = "25.1"
      , files = [ "nix/checkers/checkdoc-runner.el" ]
      , dependencies = [] : List Text
      , recipe =
          ''
          (checkdoc-runner :fetcher github :repo "akirak/melpa-check" :files ("nix/checkers/checkdoc-runner.el"))
          ''
      }
    , Package::{
      , pname = "melpazoid-misc-runner"
      , version = "0.1"
      , emacsVersion = "25.1"
      , files = [ "nix/checkers/melpazoid-misc-runner.el" ]
      , dependencies = [ "melpazoid" ]
      , localDependencies = [ "melpazoid" ]
      , recipe =
          ''
          (melpazoid-misc-runner :fetcher github :repo "akirak/melpa-check" :files ("nix/checkers/melpazoid-misc-runner.el"))
          ''
      }
    ]
