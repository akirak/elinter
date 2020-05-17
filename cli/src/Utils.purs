module Utils where

import Control.Monad.Error.Class (throwError)
import Control.Monad.Except (runExcept)
import Data.Array (cons, length, mapMaybe, updateAt, (!!))
import Data.Array as A
import Data.Array.NonEmpty as NA
import Data.Either (Either(..), either, fromRight)
import Data.List as L
import Data.Maybe (Maybe(..), fromJust, fromMaybe, isJust)
import Data.Posix.Signal (Signal(..))
import Data.String (Pattern(..), contains, joinWith, split, trim)
import Data.String.Regex (match, regex) as R
import Data.String.Regex.Flags (noFlags) as R
import Data.Traversable (sequence)
import Data.Tuple (Tuple)
import Data.Tuple as T
import Effect (Effect)
import Effect.Aff (Aff, attempt, effectCanceler, makeAff)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Class.Console (log)
import Effect.Class.Console as Console
import Effect.Exception (Error, error, throwException)
import Effect.Exception as E
import Effect.Ref (modify_, new, read)
import Foreign.Generic (decodeJSON)
import Foreign.Generic.Class (class Decode)
import Node.ChildProcess (Exit(..), SpawnOptions, StdIOBehaviour(..), defaultSpawnOptions, inherit)
import Node.ChildProcess as CP
import Node.Encoding (Encoding(UTF8))
import Node.FS (SymlinkType(DirLink, FileLink))
import Node.FS.Stats (Stats, isDirectory, isFile)
import Node.FS.Sync as FS
import Node.Path (FilePath)
import Node.Path as Path
import Node.Process (exit, lookupEnv)
import Node.Stream (onDataString)
import Partial.Unsafe (unsafePartial)
import Prelude (Unit, bind, const, discard, ifM, join, map, pure, show, unit, unlessM, void, ($), (<$>), (<*>), (<<<), (<>), (>>=), (>>>))

callProcess :: String -> Array String -> Effect Unit
callProcess cmd args = do
  _ <-
    CP.execFileSync cmd args
      ( CP.defaultExecSyncOptions
          { stdio = CP.inherit
          }
      )
  pure unit

quoteMaybe :: String -> String
quoteMaybe s
  | contains (Pattern " ") s = "'" <> s <> "'"
  | true = s

showCommand :: String -> Array String -> String
showCommand cmd args = joinWith " " $ map quoteMaybe (cons cmd args)

callProcessAsync_ :: SpawnOptions -> String -> Array String -> Aff Unit
callProcessAsync_ spawnOptions cmd args = do
  let
    command = showCommand cmd args
  -- Suppress logging.
  -- liftEffect $ log $ "> " <> command
  r <-
    makeAff
      $ \cb -> do
          p <- CP.spawn cmd args $ spawnOptions { stdio = inherit }
          CP.onError p $ cb <<< Left <<< CP.toStandardError
          CP.onExit p $ cb <<< Right
          pure <<< effectCanceler <<< void $ CP.kill SIGTERM p
  case r of
    Normally 0 -> pure unit
    Normally n -> do
      -- Console.error $ "\nExit code " <> show n
      throwError $ error $ "Exit code " <> show n <> " from command \"" <> command <> "\""
    _ -> do
      -- Console.errorShow r
      throwError $ error $ show r <> " from command " <> command

callProcessAsync :: String -> Array String -> Aff Unit
callProcessAsync = callProcessAsync_ defaultSpawnOptions

getProcessOutput :: String -> Array String -> Aff String
getProcessOutput cmd args = do
  let
    stdio = unsafePartial $ fromJust $ updateAt 1 (Just Pipe) inherit
  { exit, stdout } <-
    makeAff
      $ \cb -> do
          stdoutRef <- new ""
          p <- CP.spawn cmd args $ defaultSpawnOptions { stdio = stdio }
          onDataString (CP.stdout p) UTF8 \s ->
            modify_ (_ <> s) stdoutRef
          CP.onError p $ cb <<< Left <<< CP.toStandardError
          CP.onExit p
            $ \exit -> do
                stdout <- read stdoutRef
                cb <<< Right $ { stdout, exit }
          pure <<< effectCanceler <<< void $ CP.kill SIGTERM p
  case exit of
    Normally 0 -> pure stdout
    Normally n -> do
      -- Console.error $ "\nExit code " <> show n
      throwError $ error $ "Exit code " <> show n <> " from command \"" <> showCommand cmd args <> "\""
    _ -> do
      -- Console.errorShow r
      throwError $ error $ show exit <> " from command " <> showCommand cmd args

getProcessOutputAsJson :: forall t249. Decode t249 => String -> Array String -> Aff t249
getProcessOutputAsJson cmd args = do
  raw <- getProcessOutput cmd args
  let
    r = runExcept $ decodeJSON raw
  case r of
    Left e -> throwError $ error $ show e
    Right a -> pure a

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

examineAll :: forall t235. Array (Aff t235) -> Aff Unit
examineAll xs = do
  results <- sequence $ map attempt_ xs
  case mapMaybe (either Just (const Nothing)) results of
    [] -> pure unit
    ys -> do
      let
        msg =
          show (length ys) <> "/" <> show (length xs) <> " of the tasks have failed:\n"
            <> joinWith "\n" (map (\e -> E.message e) ys)
      throwError $ error msg
  where
  attempt_ f = do
    r <- attempt f
    log ""
    pure r

exitOnError :: forall a. Either Error a -> Effect Unit
exitOnError = either f (const $ pure unit)
  where
  f e = do
    printError e
    exit 1

printError :: forall t13. MonadEffect t13 => Error -> t13 Unit
printError e = Console.error $ E.name e <> ": " <> E.message e
