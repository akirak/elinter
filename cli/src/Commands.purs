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
import Foreign.Object (insert)
import Lib (EmacsVersion, PackageName, buttercupCommand, byteCompileCommand, checkdocCommand, doesConfigExist, ertCommand, ertRunnerCommand, getConfigPath, packageLintCommand, runInNixShell, setConfigPath, testCommand)
import Node.ChildProcess (defaultSpawnOptions)
import Node.Path as Path
import Node.Process (getEnv)
import Prelude (Unit, bind, discard, ifM, map, pure, unit, unlessM, whenM, ($), (<>))
import Utils (callProcess, callProcessAsync_, doesFileExist, exitOnError, getHomeDirectory, getProcessOutputAsJson, getSubstituters, hasExecutable, logTextFileContent, readNixConf)

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
  let
    nixConfFile = Path.concat [ home, ".config", "nix", "nix.conf" ]
  whenM (doesFileExist nixConfFile)
    $ logTextFileContent nixConfFile
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
  origEnv <- liftEffect getEnv
  let
    env = insert "NIX_BUILD_SHELL" "bash" origEnv

    spawnOptions = defaultSpawnOptions { env = Just env }
  runAff_ exitOnError
    $ callProcessAsync_ spawnOptions "nix-shell"
        [ "--quiet", configPath, "-A", "meta" ]

type LintOpts
  = { loCheckdoc :: Boolean
    , loPackageLint :: Boolean
    , loEmacsVersion :: Maybe EmacsVersion
    }

runLint :: LintOpts -> Maybe PackageName -> Effect Unit
runLint opts mPackage =
  runInNixShell
    $ catMaybes
        [ do
            guard opts.loCheckdoc
            pure $ checkdocCommand opts.loEmacsVersion mPackage
        , do
            guard opts.loPackageLint
            pure $ packageLintCommand opts.loEmacsVersion mPackage
        ]

type ByteCompileOpts
  = { emacsVersion :: Maybe EmacsVersion
    }

byteCompile :: ByteCompileOpts -> Maybe PackageName -> Effect Unit
byteCompile opts mPackage =
  runInNixShell
    [ byteCompileCommand opts.emacsVersion mPackage
    ]

type ButtercupOpts
  = { emacsVersion :: Maybe EmacsVersion
    }

runButtercup :: ButtercupOpts -> Maybe PackageName -> Effect Unit
runButtercup opts mPackage = do
  runInNixShell [ buttercupCommand opts.emacsVersion mPackage ]

type ErtOpts
  = { emacsVersion :: Maybe EmacsVersion
    }

runErt :: ErtOpts -> Maybe PackageName -> Effect Unit
runErt opts mPackage = do
  runInNixShell [ ertCommand opts.emacsVersion mPackage ]

type ErtRunnerOpts
  = { emacsVersion :: Maybe EmacsVersion
    }

runErtRunner :: ErtRunnerOpts -> Maybe PackageName -> Effect Unit
runErtRunner opts mPackage = do
  runInNixShell [ ertRunnerCommand opts.emacsVersion mPackage ]

type TestOpts
  = { emacsVersion :: Maybe EmacsVersion
    }

runTest :: TestOpts -> Maybe PackageName -> Effect Unit
runTest opts mPackage = do
  runInNixShell [ testCommand opts.emacsVersion mPackage ]

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
  = { emacsVersion :: Maybe EmacsVersion
    }

runAll :: AllOpts -> Effect Unit
runAll opts = do
  let
    withAllPackages cmd =
      "for p in ${packages[*]}; do\n"
        <> cmd opts.emacsVersion (Just "$p")
        <> "\ndone"
  runInNixShell
    $ map withAllPackages
        [ checkdocCommand
        , packageLintCommand
        , byteCompileCommand
        , testCommand
        ]
