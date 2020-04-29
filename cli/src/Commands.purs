module Commands where

import Utils
import Data.Array as A
import Data.Maybe (Maybe)
import Effect (Effect)
import Effect.Console (log)
import Effect.Exception (error, throwException)
import Lib (emacsCiChannelName, emacsCiUrl)
import Node.Path as Path
import Prelude (Unit, bind, const, discard, ifM, pure, unit, unless, unlessM, ($), (<>))

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
  logTextFileContent $ Path.concat [ home, ".nix-channels" ]
  logTextFileContent $ Path.concat [ home, ".config", "nix", "nix.conf" ]
  where
  enabledEmacsCiCache = do
    conf <- readNixConf
    pure (A.elem "https://emacs-ci.cachix.org" (getSubstituters conf))

type ConfigOpts
  = { configFile :: Maybe String
    }

checkConfig :: ConfigOpts -> Effect Unit
checkConfig opts = pure unit
