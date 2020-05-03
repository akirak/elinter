module Commands where

import Control.MonadZero (guard)
import Data.Array (catMaybes)
import Data.Array as A
import Data.Maybe (Maybe(..), maybe)
import Effect (Effect)
import Effect.Aff (runAff_)
import Effect.Console (log)
import Effect.Exception (error, throwException)
import Lib (doesConfigExist, getConfigPath, nixShell, setConfigPath)
import Node.Path as Path
import Prelude (Unit, bind, discard, ifM, pure, unit, unlessM, ($), (<>))
import Utils (callProcess, examineAll, exitOnError, getHomeDirectory, getSubstituters, hasExecutable, logTextFileContent, readNixConf)

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
    nixOptions =
      { nixFile: configPath
      }

    nixShell' = nixShell nixOptions
  -- Use nix-instantiate?
  runAff_ exitOnError $ nixShell' {} "meta"

type LintOpts
  = { loCheckdoc :: Boolean
    , loPackageLint :: Boolean
    }

type PackageName
  = String

runLint :: LintOpts -> Maybe PackageName -> Effect Unit
runLint opts mPackage = do
  configPath <- getConfigPath
  let
    nixOptions =
      { nixFile: configPath
      }

    nixShell' = nixShell nixOptions

    packageSuffix = maybe "" (\package -> "." <> package) mPackage

    tasks =
      catMaybes
        [ do
            guard opts.loCheckdoc
            pure $ nixShell' {} ("checkdoc" <> packageSuffix)
        , do
            guard opts.loPackageLint
            pure $ nixShell' {} ("package-lint" <> packageSuffix)
        ]
  runAff_ exitOnError $ examineAll tasks
