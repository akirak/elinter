module Lib where

import Utils (makeSymbolicLink)
import Effect (Effect)
import Effect.Exception (error, throwException)
import Node.FS (SymlinkType(DirLink, FileLink))
import Node.FS.Stats (isSymbolicLink)
import Node.FS.Sync as FS
import Node.Path (FilePath)
import Node.Path as Path
import Prelude (Unit, bind, discard, pure, whenM, ($), (<>))

getConfigPath :: Effect FilePath
getConfigPath = do
  hasLink <- FS.exists tempConfigPath
  if hasLink then
    FS.realpath tempConfigPath
  else
    pure defaultConfigPath

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
