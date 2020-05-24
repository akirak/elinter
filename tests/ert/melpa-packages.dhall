let Schema = ../../schema.dhall

let Package = Schema.Package

let TestDriver = Schema.TestDriver

in  [ Package::{
      , pname = "hello3"
      , version = "0.1"
      , emacsVersion = "25.1"
      , files = [ "tests/ert/hello3.el" ]
      , dependencies = [ "dash" ]
      , testDrivers = [ TestDriver.ert ]
      , ertTests = [ "tests/ert/hello3-test.el" ]
      , recipe =
          ''
          (hello3 :fetcher github :repo "akirak/melpa-check"
             :files ("tests/ert/hello3.el"))
          ''
      }
    , Package::{
      , pname = "hello4"
      , version = "0.1"
      , emacsVersion = "25.1"
      , files = [ "tests/ert/hello3.el" ]
      , testDrivers = [ TestDriver.ert ]
      , ertTests = [ "tests/ert/hello4-test.el" ]
      , dependencies = [ "dash" ]
      , recipe =
          ''
          (hello4 :fetcher github :repo "akirak/melpa-check"
             :files ("tests/ert/hello4.el"))
          ''
      }
    ]
