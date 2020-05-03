module Lib where

import Data.Array (concat)
import Data.Maybe (Maybe(..), maybe)
import Effect (Effect)
import Effect.Aff (Aff, runAff_)
import Effect.Class (liftEffect)
import Effect.Exception (error, throwException)
import Foreign.Object (insert)
import Node.ChildProcess (defaultSpawnOptions)
import Node.FS.Stats (isSymbolicLink)
import Node.FS.Sync as FS
import Node.Path (FilePath)
import Node.Path as Path
import Node.Process (cwd, getEnv)
import Prelude (Unit, bind, discard, ifM, pure, whenM, ($), (<>))
import Utils (callProcessAsync_, doesDirectoryExist, doesFileExist, examineAll, exitOnError, makeSymbolicLink)

getConfigPath :: Effect FilePath
getConfigPath = do
  hasLink <- FS.exists tempConfigPath
  workDir <- cwd
  if hasLink then do
    dest <- FS.readlink tempConfigPath
    pure $ Path.concat [ workDir, dest ]
  else
    pure $ Path.concat [ workDir, defaultConfigPath ]

defaultConfigPath :: FilePath
defaultConfigPath = ".melpa-check"

tempConfigPath :: FilePath
tempConfigPath = ".melpa-check-tmp"

setConfigPath :: FilePath -> Effect Unit
setConfigPath path = do
  whenM (FS.exists tempConfigPath)
    $ do
        stat <- FS.stat tempConfigPath
        if isSymbolicLink stat then
          FS.unlink tempConfigPath
        else
          throwException $ error $ tempConfigPath <> " already exists and is not a symbolic link"
  makeSymbolicLink tempConfigPath path

-- | Find the actual location of an existing config file.
-- |
-- | If none, return Nothing.
doesConfigExist :: FilePath -> Effect Boolean
doesConfigExist path = do
  ifM (doesDirectoryExist path)
    (doesFileExist (Path.concat [ path, "default.nix" ]))
    $ doesFileExist path

type NixOptions
  = { quiet :: Boolean
    , emacsVersion :: Maybe String
    }

defaultNixOptions :: NixOptions
defaultNixOptions =
  { quiet: true, emacsVersion: Nothing
  }

nixOptionsToArray :: NixOptions -> Array String
nixOptionsToArray opts =
  concat
    [ if opts.quiet then
        [ "--quiet" ]
      else
        []
    , case opts.emacsVersion of
        Just version -> [ "--argstr", "emacs", version ]
        Nothing -> []
    ]

type NixShellOptions
  = { clearEnv :: Boolean
    }

defaultNixShellOptions :: NixShellOptions
defaultNixShellOptions = { clearEnv: true }

nixShellOptionsToArray :: NixShellOptions -> Array String
nixShellOptionsToArray opts =
  concat
    [ if opts.clearEnv then [ "--pure" ] else []
    ]

type NixBuildOptions
  = { noOutLink :: Boolean
    , noBuildOutput :: Boolean
    }

defaultNixBuildOptions :: NixBuildOptions
defaultNixBuildOptions = { noOutLink: true, noBuildOutput: false }

nixBuildOptionsToArray :: NixBuildOptions -> Array String
nixBuildOptionsToArray opts =
  concat
    [ if opts.noOutLink then [ "--no-out-link" ] else []
    , if opts.noBuildOutput then [ "--no-build-output" ] else []
    ]

type AttrPath
  = String

nixShell :: FilePath -> NixOptions -> NixShellOptions -> String -> Aff Unit
nixShell nixFile nixOpts nixShOpts attrPath = do
  origEnv <- liftEffect getEnv
  let
    env = insert "NIX_BUILD_SHELL" "bash" origEnv

    spawnOptions = defaultSpawnOptions { env = Just env }
  callProcessAsync_ spawnOptions "nix-shell"
    $ nixShellOptionsToArray nixShOpts
    <> [ "-A", attrPath ]
    <> nixOptionsToArray nixOpts
    <> [ nixFile ]

nixBuild :: FilePath -> NixOptions -> NixBuildOptions -> String -> Aff Unit
nixBuild nixFile nixOpts nixBuildOpts attrPath = do
  let
    spawnOptions = defaultSpawnOptions
  callProcessAsync_ spawnOptions "nix-build"
    $ nixBuildOptionsToArray nixBuildOpts
    <> [ "-A", attrPath ]
    <> nixOptionsToArray nixOpts
    <> [ nixFile ]

type TaskBuilder
  = { nixShellTask :: NixOptions -> NixShellOptions -> String -> Aff Unit
    , nixBuildTask :: NixOptions -> NixBuildOptions -> String -> Aff Unit
    }

type PackageName
  = String

runPackageTasks ::
  forall a.
  Maybe PackageName ->
  ( TaskBuilder -> Array (Aff a)
  ) ->
  Effect Unit
runPackageTasks = runPackageTasks_ examineAll

runPackageTasks_ ::
  forall a.
  (Array (Aff a) -> Aff Unit) ->
  Maybe PackageName ->
  ( TaskBuilder ->
    Array (Aff a)
  ) ->
  Effect Unit
runPackageTasks_ f mPackage makeTasks = do
  configPath <- getConfigPath
  let
    nixShell' = nixShell configPath

    nixBuild' = nixBuild configPath

    packageSuffix = maybe "" (\package -> "." <> package) mPackage

    taskBuilder =
      { nixShellTask: \opts shOpts name -> nixShell' opts shOpts (name <> packageSuffix)
      , nixBuildTask: \opts bldOpts name -> nixBuild' opts bldOpts (name <> packageSuffix)
      }

    tasks = makeTasks taskBuilder
  runAff_ exitOnError $ f tasks
