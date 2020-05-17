module Lib where

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
import Prelude (Unit, bind, discard, ifM, map, pure, whenM, ($), (<>))
import Utils (callProcessAsync_, doesDirectoryExist, doesFileExist, examineAll, exitOnError, makeSymbolicLink)

data EmacsVersion
  = Release String
  | Snapshot
  | AllSupportedVersions
  | LatestRelease
  | MinimumSupported

toVersionArg :: EmacsVersion -> String
toVersionArg (Release ver) = ver

toVersionArg Snapshot = "snapshot"

toVersionArg LatestRelease = "latest"

-- This should cause an error in Nix
toVersionArg MinimumSupported = "ERROR"

toVersionArg AllSupportedVersions = "ERROR"

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

type PackageName
  = String

runInNixShell :: Array String -> Effect Unit
runInNixShell = runInNixShell_ examineAll

runInNixShell_ :: forall t106. (Array (Aff Unit) -> Aff t106) -> Array String -> Effect Unit
runInNixShell_ f commands = do
  configPath <- getConfigPath
  origEnv <- liftEffect getEnv
  let
    env = insert "NIX_BUILD_SHELL" "bash" origEnv

    spawnOptions = defaultSpawnOptions { env = Just env }

    run command =
      callProcessAsync_ spawnOptions "nix-shell"
        [ "--quiet", configPath, "-A", "shellWithoutBuild", "--run", "set -e; " <> command ]
  runAff_ exitOnError $ f $ map run commands

generateInternalBlock :: String -> String -> Maybe String -> Maybe EmacsVersion -> Maybe String -> String
generateInternalBlock funcName expName mOptions (Just AllSupportedVersions) (Just package) =
  "for v in `packageEmacsVersions " <> package <> "`; do\n"
    <> "  "
    <> funcName
    <> maybe "" (\args -> " " <> args) mOptions
    <> " --argstr emacs $v -A "
    <> expName
    <> "."
    <> package
    <> "\ndone"

generateInternalBlock funcName expName mOptions (Just AllSupportedVersions) Nothing =
  "if [[ ${#packages[*]} -eq 1 ]]; then\n"
    <> "  p=${packages[0]}\n"
    <> "  for v in `packageEmacsVersions $p`; do\n"
    <> "  "
    <> funcName
    <> maybe "" (\args -> " " <> args) mOptions
    <> " --argstr emacs $v -A "
    <> expName
    <> "\n  done\n"
    <> "else\n"
    <> "  echo 'You have to specify a package name to run it on all supported versions'\n"
    <> "  echo 'when you have multiple packages.'\n"
    <> "  exit 2\n"
    <> "fi"

generateInternalBlock funcName expName mOptions (Just MinimumSupported) (Just package) =
  funcName
    <> maybe "" (\args -> " " <> args) mOptions
    <> " --argstr emacs ${packageEmacsVersion["
    <> package
    <> "]}"
    <> " -A "
    <> expName
    <> "."
    <> package

generateInternalBlock funcName expName mOptions (Just MinimumSupported) Nothing =
  "if [[ ${#packages[*]} -eq 1 ]]; then\n"
    <> "  p=${packages[0]}\n"
    <> "  "
    <> funcName
    <> maybe "" (\args -> " " <> args) mOptions
    <> " --argstr emacs ${packageEmacsVersion[$p]}"
    <> " -A "
    <> expName
    <> "\n"
    <> "else\n"
    <> "  echo 'You have to specify a package name to run it on the minimum supported version.'\n"
    <> "  echo 'when you have multiple packages.'\n"
    <> "  exit 2\n"
    <> "fi"

generateInternalBlock funcName expName mOptions mVer mPackage =
  funcName
    <> maybe "" (\args -> " " <> args) mOptions
    <> maybe "" (\ver -> " --argstr emacs " <> toVersionArg ver) mVer
    <> " -A "
    <> expName
    <> maybe "" (\package -> "." <> package) mPackage

innerBuilder :: String
innerBuilder = "melpaCheckNixBuild"

innerShell :: String
innerShell = "melpaCheckNixShell"

checkdocCommand :: Maybe EmacsVersion -> Maybe String -> String
checkdocCommand = generateInternalBlock innerShell "checkdoc" Nothing

packageLintCommand :: Maybe EmacsVersion -> Maybe String -> String
packageLintCommand = generateInternalBlock innerShell "package-lint" Nothing

byteCompileCommand :: Maybe EmacsVersion -> Maybe String -> String
byteCompileCommand = generateInternalBlock innerBuilder "byte-compile" Nothing

buttercupCommand :: Maybe EmacsVersion -> Maybe String -> String
buttercupCommand mVer mPackage =
  generateInternalBlock innerBuilder "prepareButtercup" (Just "--no-build-output") mVer mPackage
    <> " && "
    <> generateInternalBlock innerShell "buttercup" Nothing mVer mPackage
