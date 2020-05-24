let Schema = ../schema.dhall

let Package = Schema.Package

let TestDriver = Schema.TestDriver

in  [ Package::{
      , pname = "bad-hello"
      , version = "0.1"
      , emacsVersion = "25.1"
      , files = [ "tests/bad-hello.el" ]
      , dependencies = [] : List Text
      , testDrivers = [ TestDriver.buttercup ]
      , buttercupTests = Schema.noTests
      , recipe =
          ''
          (bad-hello :fetcher github :repo "akirak/emacs-package-checker"
          :files ("tests/bad-hello.el"))
          ''
      }
    ]
