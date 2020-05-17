let schema = ../schema.dhall

let Package = schema.Package.Type

let Prelude/List/map =
      https://raw.githubusercontent.com/dhall-lang/dhall-lang/v16.0.0/Prelude/List/map

let Prelude/List/concat =
      https://raw.githubusercontent.com/dhall-lang/dhall-lang/v16.0.0/Prelude/List/concat

let not =
      https://raw.githubusercontent.com/dhall-lang/dhall-lang/v16.0.0/Prelude/Bool/not

let null =
      https://raw.githubusercontent.com/dhall-lang/dhall-lang/v16.0.0/Prelude/List/null

let concat = Prelude/List/concat

let EventType =
      < published
      | unpublished
      | created
      | edited
      | deleted
      | prereleased
      | assigned
      | opened
      | synchronize
      | reopened
      >

let GitEvent =
      { Type =
          { types : Optional (List EventType)
          , paths : Optional (List Text)
          , paths-ignore : Optional (List Text)
          , tags : Optional (List Text)
          , tags-ignore : Optional (List Text)
          , branches : Optional (List Text)
          , branches-ignore : Optional (List Text)
          }
      , default =
        { types = None (List EventType)
        , paths = None (List Text)
        , paths-ignore = None (List Text)
        , tags = None (List Text)
        , tags-ignore = None (List Text)
        , branches = None (List Text)
        , branches-ignore = None (List Text)
        }
      }

let On =
      { Type =
          { push : Optional GitEvent.Type
          , pull_request : Optional GitEvent.Type
          }
      , default =
        { push = Some GitEvent::{
          , paths-ignore = Some [ "**/*.md", "**/*.org" ]
          }
        , pull_request = None GitEvent.Type
        }
      }

let OS = < ubuntu-latest | macos-latest >

let Step =
      < Uses : { uses : Text } | Run : { name : Optional Text, run : Text } >

let Job = { runs-on : List OS, steps : List Step }

let Workflow = { name : Text, on : On.Type, jobs : { lint : Job, tests : Job } }

let MultiFileCiConfig =
      { Type =
          { triggers : Package → On.Type
          , lintOn : List OS
          , lintEmacsVersion : Text
          , testOn : List OS
          , testEmacsVersion : Text
          , fileNameFn : Text → Text
          , actionNameFn : Text → Text
          , skipTests : Bool
          }
      , default =
        { triggers = λ(_ : Package) → On.default
        , lintOn = [ OS.ubuntu-latest ]
        , lintEmacsVersion = "latest"
        , testOn = [ OS.ubuntu-latest ]
        , testEmacsVersion = "all"
        , fileNameFn = λ(pname : Text) → pname
        , actionNameFn = λ(pname : Text) → pname ++ " CI"
        , skipTests = False
        }
      }

let WorkflowFile = { fileName : Text, content : Workflow }

let uses = λ(repo : Text) → Step.Uses { uses = repo }

let installNix = uses "cachix/install-nix-action@v8"

let checkout = uses "actions/checkout@v1"

let configPackage = uses "akirak/emacs-package@master"

let runWithName =
        λ(name : Text)
      → λ(command : Text)
      → Step.Run { name = Some name, run = command }

let melpaCheckCommandWithOpts =
        λ(subcommand : Text)
      → λ(options : Text)
      → λ(pname : Text)
      → "melpa-check " ++ subcommand ++ " " ++ options ++ " " ++ pname

let initSteps = [ installNix, checkout, configPackage ]

let toMultiFileCiWorkflow =
        λ(config : MultiFileCiConfig.Type)
      → λ(package : Package)
      → let fileName =
              ".github/workflows/" ++ config.fileNameFn package.pname ++ ".yml"

        let name = config.actionNameFn package.pname

        let on = config.triggers package

        let withEmacsVersion =
                λ(emacsVersion : Text)
              → λ(name : Text)
              → λ(command : Text)
              →   runWithName
                    (name ++ " (with Emacs version: " ++ emacsVersion ++ ")")
                    ( melpaCheckCommandWithOpts
                        command
                        ("-e " ++ emacsVersion)
                        package.pname
                    )
                : Step

        let lintSteps =
              [ withEmacsVersion config.lintEmacsVersion "Lint" "lint" ]

        let lint =
              { runs-on = config.lintOn
              , steps = concat Step [ initSteps, lintSteps ]
              }

        let buildSteps =
              [ withEmacsVersion
                  config.testEmacsVersion
                  "Byte-compile"
                  "byte-compile"
              ]

        let testsWanted =
              not (config.skipTests || null Text package.buttercupTests)

        let testSteps =
                    if testsWanted

              then  [ withEmacsVersion
                        config.testEmacsVersion
                        "Test using buttercup"
                        "buttercup"
                    ]

              else  [] : List Step

        let tests =
              { runs-on = config.testOn
              , steps = concat Step [ initSteps, buildSteps, testSteps ]
              }

        in    { fileName, content = { name, on, jobs = { lint, tests } } }
            : WorkflowFile

let buildMultiFileCiWorkflows =
        λ(config : MultiFileCiConfig.Type)
      → λ(packages : List Package)
      → { directories = [ ".github/workflows" ]
        , files =
            Prelude/List/map
              Package
              WorkflowFile
              (toMultiFileCiWorkflow config)
              packages
        }

in  { Package
    , EventType
    , GitEvent
    , On
    , OS
    , Step
    , Job
    , Workflow
    , WorkflowFile
    , MultiFileCiConfig
    , buildMultiFileCiWorkflows
    }
