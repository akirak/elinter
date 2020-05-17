module Main where

import Version (versionString)
import Commands
import Control.Applicative ((<$>), (<*>))
import Data.Maybe (Maybe, optional)
import Effect (Effect)
import Lib (EmacsVersion(..))
import Options.Applicative (Parser, ReadM, command, execParser, flag, help, helper, hidden, info, infoOption, long, metavar, option, progDesc, short, strArgument, strOption, subparser, (<**>))
import Options.Applicative.Types (readerAsk)
import Prelude (Unit, join, pure, ($), (<>), bind)
import Record.Extra (sequenceRecord)

main :: Effect Unit
main = join $ execParser (info (opts <**> helper <**> showVersion) progInfo)
  where
  progInfo = progDesc "CLI frontend for melpa-check, an Emacs Lisp package lint runner"

  showVersion =
    infoOption ("melpa-check CLI " <> versionString)
      ( long "version"
          <> short 'V'
          <> help "Show version"
          <> hidden
      )

opts :: Parser (Effect Unit)
opts =
  subparser
    ( command "deps" (info_ (pure installDeps) (progDesc "Install dependencies"))
        <> command "config" (info_ (checkConfig <$> configOpts) (progDesc "Set up an entry point and check the configuration"))
        <> command "lint" (info_ (runLint <$> lintOpts <*> packageArg) (progDesc "Run lint (i.e. checkdoc, package-lint, etc.) on a package"))
        <> command "byte-compile" (info_ (byteCompile <$> byteCompileOpts <*> packageArg) (progDesc "Byte-Compile packages"))
        <> command "buttercup" (info_ (runButtercup <$> buttercupOpts <*> packageArg) (progDesc "Run buttercup tests"))
        <> command "list" (info_ listCommands (progDesc "Display a list of certain things in the project"))
        <> command "all" (info_ (runAll <$> allOpts) (progDesc "Run all tasks on all packages"))
    )
  where
  listCommands =
    subparser
      ( command "emacs-versions" (info_ (pure listEmacsVersions) (progDesc "Display a list of Emacs versions"))
          <> command "packages" (info_ (pure listPackages) (progDesc "Display a list of packages under test"))
      )

  info_ a b = info (a <**> helper) b

  configOpts =
    sequenceRecord
      { configFile: mFile
      }

  lintOpts =
    sequenceRecord
      { loCheckdoc:
          flag true false
            ( long "no-checkdoc"
                <> help "Don't run checkdoc"
            )
      , loPackageLint:
          flag true false
            ( long "no-package-lint"
                <> help "Don't run package-lint"
            )
      , loEmacsVersion: emacsArg
      }

  byteCompileOpts =
    sequenceRecord
      { emacsVersion: emacsArg
      }

  buttercupOpts =
    sequenceRecord
      { emacsVersion: emacsArg
      }

  allOpts =
    sequenceRecord
      { emacsVersion: emacsArg
      }

  mFile :: Parser (Maybe String)
  mFile =
    optional
      $ strOption
          ( long "file"
              <> short 'f'
              <> metavar "FILE"
              <> help "Path to configuration file/directory"
          )

  packageArg = optional $ strArgument (metavar "PACKAGE")

  emacsArg =
    optional
      $ option readEmacsVersion
          (long "emacs" <> short 'e' <> metavar "[RELEASE|snapshot|latest|minimum|all]" <> help "Emacs version to use for the task")

readEmacsVersion :: ReadM EmacsVersion
readEmacsVersion = do
  raw <- readerAsk
  case raw of
    "snapshot" -> pure Snapshot
    "latest" -> pure LatestRelease
    "minimum" -> pure MinimumSupported
    "all" -> pure AllSupportedVersions
    -- Otherwise it is considered a release version (XX.X)
    _ -> pure $ Release raw
