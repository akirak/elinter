module Lib where

import Effect (Effect)
import Effect.Exception (error, throwException)
import Node.FS.Stats (isSymbolicLink)
import Node.FS.Sync as FS
import Node.Path (FilePath)
import Node.Path as Path
import Node.Process (cwd)
import Prelude (Unit, bind, discard, ifM, pure, whenM, ($), (<>))
import Utils (makeSymbolicLink, callProcess, doesFileExist, doesDirectoryExist)

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
  = { nixFile :: FilePath
    }

nixOptionsToArray :: NixOptions -> Array String
nixOptionsToArray opts = [ opts.nixFile ]

type NixShellOptions
  = {}

nixShellOptionsToArray :: NixShellOptions -> Array String
nixShellOptionsToArray _ = []

type AttrPath
  = String

nixShell :: NixOptions -> NixShellOptions -> AttrPath -> Effect Unit
nixShell nixOpts nixShOpts attrPath =
  callProcess "nix-shell"
    $ nixShellOptionsToArray nixShOpts
    <> [ "-A", attrPath ]
    <> nixOptionsToArray nixOpts
