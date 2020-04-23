module Main where

import Utils (callProcess, hasExecutable, getSubstituters, readNixConf, getNixChannels, getHomeDirectory, logTextFileContent)
import Commands
import Prelude (Unit, join, pure, ($), (<*>), discard, unlessM, bind, ifM, (<>), unless)
import Data.Array as A
import Effect (Effect)
import Effect.Console (log)
import Effect.Exception (error, throwException)
import Node.Path as Path
import Options.Applicative (Parser, (<**>), command, short, long, help, hidden, execParser, idm, info, subparser, helper, progDesc, prefs, showHelpOnEmpty, infoOption)

main :: Effect Unit
main = join $ execParser (info (opts <**> helper <**> showVersion) progInfo)

progInfo = progDesc "CLI frontend for melpa-check, an Emacs Lisp package lint runner"

-- TODO: Make the version number consistent
versionString = "0.1"

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
        <> command "config" (info (pure checkConfig) (progDesc "Set up an entry point and check the configuration"))
    )
