let Schema = ../schema.dhall

let Package = Schema.Package

let TestDriver = Schema.TestDriver

in  [ Package::{
      , pname = "melpa-check"
      , version = "0.1"
      , emacsVersion = "26.1"
      , files =
        [ "melpa-check.el"
        , "melpa-check-git.el"
        , "melpa-check-multisel.el"
        , "melpa-check-package.el"
        , "melpa-check-verify.el"
        ]
      , dependencies = [ "f", "dash", "s" ]
      , buttercupTests = [ "*-test?(s).el" ]
      , testDrivers = [] : List TestDriver
      , mainFile = Some "melpa-check.el"
      , recipe =
          ''
          (melpa-check :fetcher github :repo "akirak/melpa-check")
          ''
      }
    ]
