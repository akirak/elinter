module Commands where

import Control.MonadZero (guard)
import Data.Array (catMaybes)
import Data.Array as A
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (runAff_)
import Effect.Console (log)
import Effect.Exception (error, throwException)
import Lib (NixBuildOptions(..), NixOptions(..), NixShellOptions(..), PackageName, defaultNixBuildOptions, defaultNixOptions, defaultNixShellOptions, doesConfigExist, getConfigPath, nixShell, runPackageTasks, setConfigPath)
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
