let SchemaVersion = "1.1.1"

let PackageName
    : Type
    = Text

let File
    : Type
    = Text

let Recipe
    : Type
    = Text

let Version
    : Type
    = Text

let Pattern
    : Type
    = Text

let TestDriver
    : Type
    = < ert | ert-runner | buttercup >

let defaultTests
    : List Pattern
    = [ "test?(s).el", "test-*.el", "*-test?(s).el", "test?(s)/*.el" ]

in  { SchemaVersion
    , Package =
      { Type =
          { pname : PackageName
          , version : Version
          , files : List File
          , emacsVersion : Version
          , localDependencies : List PackageName
          , dependencies : List PackageName
          , testDrivers : List TestDriver
          , ertTests : List Pattern
          , buttercupTests : List Pattern
          , testDependencies : List PackageName
          , testExcludes : List Pattern
          , mainFile : Optional File
          , recipe : Recipe
          }
      , default =
        { localDependencies = [] : List File
        , mainFile = None File
        , testDrivers = [] : List TestDriver
        , ertTests = defaultTests
        , buttercupTests = defaultTests
        , testDependencies = [] : List PackageName
        , testExcludes = [] : List Pattern
        }
      }
    , noTests = [] : List Pattern
    , defaultTests = defaultTests : List Pattern
    , TestDriver
    }
