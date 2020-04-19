let Package = (../schema.dhall).Package

in  [ Package::{
      , pname = "bad-hello"
      , version = "0.1"
      , files = [ "tests/bad-hello.el" ]
      , dependencies = [] : List Text
      , recipe =
          ''
          (bad-hello :fetcher github :repo "akirak/emacs-package-checker"
          :files ("tests/bad-hello.el"))
          ''
      }
    ]
