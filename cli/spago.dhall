{ name = "melpa-check"
, dependencies =
  [ "console"
  , "effect"
  , "arrays"
  , "exceptions"
  , "psci-support"
  , "optparse"
  , "node-child-process"
  , "record-extra"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
