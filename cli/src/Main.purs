module Main where

import Commands
import Control.Applicative ((<$>), (<*>))
import Data.Maybe (Maybe, optional)
import Effect (Effect)
import Options.Applicative (Parser, command, execParser, flag, help, helper, hidden, info, infoOption, long, metavar, progDesc, short, strArgument, strOption, subparser, (<**>))
import Prelude (Unit, join, pure, ($), (<>))
import Record.Extra (sequenceRecord)

-- TODO: Make the version number consistent
versionString :: String
versionString = "0.1"

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
    ( command "deps" (info (pure installDeps) (progDesc "Install dependencies"))
        <> command "config" (info (checkConfig <$> configOpts) (progDesc "Set up an entry point and check the configuration"))
        <> command "lint" (info (runLint <$> lintOpts <*> packageArg) (progDesc "Lint"))
    -- TODO: byte-compile
    -- TODO: test
    )
  where
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

  packageArg = strArgument (metavar "PACKAGE")
