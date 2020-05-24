let Schema = ../schema.dhall

let Package = Schema.Package

let TestDriver = Schema.TestDriver

in  [ Package::{
      , pname = "hello"
      , version = "0.1"
      , emacsVersion = "25.1"
      , files = [ "tests/hello.el", "tests/hello-util.el" ]
      , dependencies = [ "dash" ]
      , testDrivers = [ TestDriver.buttercup ]
      , mainFile = Some "tests/hello.el"
      , buttercupTests = [ "tests/hello-test?(s).el", "tests/tests/*.el" ]
      , recipe =
          ''
          (hello :fetcher github :repo "akirak/emacs-package-checker"
             :files ("tests/hello.el" "tests/hello-util.el"))
          ''
      }
    , Package::{
      , pname = "hello2"
      , version = "0.1"
      , emacsVersion = "25.1"
      , files = [ "tests/hello2.el" ]
      , dependencies = [ "hello" ]
      , localDependencies = [ "hello" ]
      , testDrivers = [ TestDriver.buttercup ]
      , buttercupTests = [ "tests/hello2-test?(s).el" ]
      , recipe =
          ''
          (hello2 :fetcher github :repo "akirak/emacs-package-checker"
             :files ("tests/hello2.el"))
          ''
      }
    ]
