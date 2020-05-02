module Utils where

import Data.Array ((!!))
import Data.Array as A
import Data.Array.NonEmpty as NA
import Data.Either (fromRight)
import Data.List as L
import Data.Maybe (Maybe(..), fromMaybe, isJust)
import Data.String (split, Pattern(..), trim)
import Data.String.Regex (match, regex) as R
import Data.String.Regex.Flags (noFlags) as R
import Data.Tuple (Tuple)
import Data.Tuple as T
import Effect (Effect)
import Effect.Console (log)
import Effect.Exception (error, throwException)
import Node.ChildProcess as CP
import Node.Encoding (Encoding(UTF8))
import Node.FS (SymlinkType(DirLink, FileLink))
import Node.FS.Stats (Stats, isDirectory, isFile)
import Node.FS.Sync as FS
import Node.Path (FilePath)
import Node.Path as Path
import Node.Process (cwd, lookupEnv)
import Partial.Unsafe (unsafePartial)
import Prelude (Unit, bind, discard, ifM, join, map, pure, unit, unlessM, ($), (<$>), (<*>), (<<<), (<>), (>>=), (>>>))

callProcess :: String -> Array String -> Effect Unit
callProcess cmd args = do
  _ <-
    CP.execFileSync cmd args
      ( CP.defaultExecSyncOptions
          { stdio = CP.inherit
          }
      )
  pure unit

hasExecutable :: String -> Effect Boolean
hasExecutable program = do
  mBin <- findExecutable program
  pure $ isJust mBin

findExecutable :: String -> Effect (Maybe String)
findExecutable program =
  if Path.isAbsolute program then do
    executable <- isExecutable program
    if executable then
      pure (Just program)
    else
      pure Nothing
  else do
    dirs <- getPathDirectories
    L.foldM go Nothing (L.fromFoldable dirs)
  where
  go mPath dir = case mPath of
    Just path -> pure $ Just path
    Nothing -> do
      let
        path = Path.concat [ dir, program ]
      executable <- isExecutable path
      if executable then
        pure (Just path)
      else
        pure Nothing

isExecutable :: String -> Effect Boolean
isExecutable file = do
  -- TODO: Check executable permission
  exist <- FS.exists file
  if exist then do
    stats <- FS.stat file
    pure $ isFile stats
  else
    pure false

getPathDirectories :: Effect (Array String)
getPathDirectories = do
  mPath <- lookupEnv "PATH"
  case mPath of
    Nothing -> pure []
    Just rawPath -> pure (split (Pattern ":") rawPath)

type ConfEntry
  = Tuple String String

type NixConf
  = Array ConfEntry

readNixConf :: Effect NixConf
readNixConf = do
  confFile <- getNixConfFile
  exist <- FS.exists confFile
  if exist then do
    src <- FS.readTextFile UTF8 confFile
    pure (parseNixConf src)
  else
    pure []

parseNixConf :: String -> NixConf
parseNixConf =
  let
    regexp =
      unsafePartial $ fromRight
        $ R.regex "^([^#]\\w+)\\s*=\\s*(.+)$" R.noFlags

    parseEntry :: String -> Maybe ConfEntry
    parseEntry l =
      R.match regexp l
        >>= (pure <<< NA.toArray)
        >>= \a ->
            T.Tuple
              <$> (map trim $ join $ a !! 1)
              <*> (map trim $ join $ a !! 2)
  in
    split (Pattern "\n")
      >>> A.mapMaybe parseEntry

lookupConf :: String -> NixConf -> Maybe String
lookupConf key = T.lookup key

getSubstituters :: NixConf -> Array String
getSubstituters =
  lookupConf "substituters"
    >>> map (split (Pattern " "))
    >>> fromMaybe []

getNixConfFile :: Effect String
getNixConfFile = do
  xdgConf <- lookupEnv "XDG_CONFIG_HOME"
  case xdgConf of
    Just confDir -> pure (Path.concat [ confDir, "nix", "nix.conf" ])
    Nothing -> do
      home <- getHomeDirectory
      pure (Path.concat [ home, ".config", "nix", "nix.conf" ])

getHomeDirectory :: Effect String
getHomeDirectory = do
  mHome <- lookupEnv "HOME"
  case mHome of
    Just home -> pure home
    Nothing -> throwException $ error "No HOME environment variable"

logTextFileContent :: String -> Effect Unit
logTextFileContent filepath = do
  log ""
  log "--------------------------------------------------"
  log filepath
  log "--------------------------------------------------"
  content <- FS.readTextFile UTF8 filepath
  log content

makeSymbolicLink :: FilePath -> FilePath -> Effect Unit
makeSymbolicLink src dest = do
  unlessM (FS.exists dest)
    $ throwException
    $ error
    $ dest
    <> " does not exist"
  stat <- FS.stat dest
  let
    linkType =
      if isFile stat then
        FileLink
      else
        DirLink
  FS.symlink dest src linkType

withStat :: forall t4. String -> (Stats -> t4) -> Effect t4
withStat path f = do
  stat <- FS.stat path
  pure $ f stat

doesFileExist :: String -> Effect Boolean
doesFileExist path =
  ifM (FS.exists path)
    (withStat path isFile)
    $ pure false

doesDirectoryExist :: String -> Effect Boolean
doesDirectoryExist path =
  ifM (FS.exists path)
    (withStat path isDirectory)
    $ pure false
