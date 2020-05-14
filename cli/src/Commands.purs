module Commands where

import Control.MonadZero (guard)
import Data.Array (catMaybes)
import Data.Array as A
import Data.Maybe (Maybe(..))
import Data.Traversable (sequence_)
import Effect (Effect)
import Effect.Aff (runAff_)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Effect.Exception (error, throwException)
import Lib (PackageName, defaultNixBuildOptions, defaultNixOptions, defaultNixShellOptions, doesConfigExist, getConfigPath, nixShell, runPackageTasks, runPackageTasks_, setConfigPath)
import Node.Path as Path
import Prelude (Unit, bind, discard, ifM, map, pure, unit, unlessM, ($), (<>))
import Utils (callProcess, examineAll, exitOnError, getHomeDirectory, getProcessOutputAsJson, getSubstituters, hasExecutable, logTextFileContent, readNixConf)

installDeps :: Effect Unit
installDeps = do
  -- Check if nix-env is installed
  unlessM (hasExecutable "nix-env")
    $ throwException
    $ error "nix is not installed"
  -- Install cachix
  unlessM (hasExecutable "cachix")
    $ callProcess "nix-env"
        [ "-iA"
        , "cachix"
        , "-f"
        , "https://cachix.org/api/v1/install"
        ]
  -- Enable cachix for emacs-ci
  ifM enabledEmacsCiCache
    (log "Build cache for emacs-ci is already turned on.")
    $ callProcess "cachix" [ "use", "emacs-ci" ]
  -- Show information of the environment
  log "=================================================="
  log "Version information"
  log "=================================================="
  callProcess "nix-env" [ "--version" ]
  callProcess "cachix" [ "--version" ]
  home <- getHomeDirectory
  logTextFileContent $ Path.concat [ home, ".config", "nix", "nix.conf" ]
  where
  enabledEmacsCiCache = do
    conf <- readNixConf
    pure (A.elem "https://emacs-ci.cachix.org" (getSubstituters conf))

type ConfigOpts
  = { configFile :: Maybe String
    }

checkConfig :: ConfigOpts -> Effect Unit
checkConfig opts = do
  case opts.configFile of
    Nothing -> pure unit
    Just path -> do
      log $ "Set the configuration path to " <> path
      setConfigPath path
  configPath <- getConfigPath
  unlessM (doesConfigExist configPath)
    $ throwException
    $ error
    $ "Config file does not exist at "
    <> configPath
  log $ "Configuration is found at " <> configPath
  let
    nixShell' = nixShell configPath
  -- Use nix-instantiate?
  runAff_ exitOnError $ nixShell' defaultNixOptions defaultNixShellOptions "meta"

type LintOpts
  = { loCheckdoc :: Boolean
    , loPackageLint :: Boolean
    }

runLint :: LintOpts -> Maybe PackageName -> Effect Unit
runLint opts mPackage =
  runPackageTasks mPackage
    $ \builder ->
        catMaybes
          [ do
              guard opts.loCheckdoc
              pure $ builder.nixShellTask defaultNixOptions defaultNixShellOptions "checkdoc"
          , do
              guard opts.loPackageLint
              pure $ builder.nixShellTask defaultNixOptions defaultNixShellOptions "package-lint"
          ]

type ByteCompileOpts
  = { emacsVersion :: Maybe String
    }

byteCompile :: ByteCompileOpts -> Maybe PackageName -> Effect Unit
byteCompile opts mPackage =
  runPackageTasks mPackage
    $ \builder ->
        [ builder.nixBuildTask
            ( defaultNixOptions
                { emacsVersion = opts.emacsVersion
                }
            )
            defaultNixBuildOptions
            "byte-compile"
        ]

type ButtercupOpts
  = { emacsVersion :: Maybe String
    }

runButtercup :: ButtercupOpts -> Maybe PackageName -> Effect Unit
runButtercup opts mPackage = do
  let
    nixOptions =
      defaultNixOptions
        { emacsVersion = opts.emacsVersion
        }
  -- The test task depends on the build task, so they are run separately.
  runPackageTasks_ sequence_ mPackage
    $ \builder ->
        [ builder.nixBuildTask nixOptions
            ( defaultNixBuildOptions
                { noBuildOutput = true }
            )
            "prepareButtercup"
        , builder.nixShellTask nixOptions defaultNixShellOptions "buttercup"
        ]

listEmacsVersions :: Effect Unit
listEmacsVersions = do
  configPath <- getConfigPath
  runAff_ exitOnError
    $ do
        versions :: Array String <-
          getProcessOutputAsJson "nix-instantiate"
            [ "--eval", "-A", "emacsVersions", "--strict", "--json", configPath
            ]
        liftEffect $ sequence_
          $ map log versions

listPackages :: Effect Unit
listPackages = do
  configPath <- getConfigPath
  runAff_ exitOnError
    $ do
        versions :: Array String <-
          getProcessOutputAsJson "nix-instantiate"
            [ "--eval", "-A", "packageNames", "--strict", "--json", configPath
            ]
        liftEffect $ sequence_
          $ map log versions

type AllOpts
  = { emacsVersion :: Maybe String
    }

runAll :: AllOpts -> Effect Unit
runAll opts = do
  configPath <- getConfigPath
  let
    nixOpts =
      { quiet: true
      -- It doesn't mattter here, since we won't start Emacs in the parent shell
      , emacsVersion: Nothing
      }

    nixShOpts command =
      { clearEnv: false -- Inherit environment variables such as NIX_PATH
      , runNonInteractiveCommand: Just command
      , runInteractiveCommand: Nothing
      }

    run command = nixShell configPath nixOpts (nixShOpts command) "shellWithoutBuild"

    defaultArgs = case opts.emacsVersion of
      Just version -> "--argstr emacs " <> version <> " "
      Nothing -> ""

    buildAll arg = "set -e; for p in $packages; do melpaCheckNixBuild " <> defaultArgs <> arg <> "; done"

    shellAll arg = "set -e; for p in $packages; do melpaCheckNixShell " <> defaultArgs <> arg <> "; done"
  runAff_ exitOnError
    $ examineAll
        [ run $ shellAll "-A checkdoc.$p"
        , run $ shellAll "-A package-lint.$p"
        , run $ buildAll "-A byte-compile.$p"
        , run $ buildAll "-A prepareButtercup.$p --no-build-output"
        , run $ shellAll "-A buttercup.$p"
        ]
