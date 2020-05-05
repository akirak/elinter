let S = ../schema.dhall

let Package = S.Package

in  [ Package::{
      , pname = "bad-hello"
      , version = "0.1"
      , emacsVersion = "25.1"
      , files = [ "tests/bad-hello.el" ]
      , dependencies = [] : List Text
      , buttercupTests = S.noTests
      , recipe =
          ''
          (bad-hello :fetcher github :repo "akirak/emacs-package-checker"
          :files ("tests/bad-hello.el"))
          ''
      }
    ]
