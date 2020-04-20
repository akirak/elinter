module Main where

import Utils (callProcess, hasExecutable, getSubstituters, readNixConf, getNixChannels, getHomeDirectory, logTextFileContent)
import Commands
import Prelude (Unit, join, pure, ($), (<*>), discard, unlessM, bind, ifM, (<>), unless)
import Data.Array as A
import Effect (Effect)
import Effect.Console (log)
import Effect.Exception (error, throwException)
import Node.Path as Path
import Options.Applicative (Parser, command, execParser, idm, info, subparser, helper, progDesc)

main :: Effect Unit
main = join $ execParser (info (helper <*> opts) idm)

opts :: Parser (Effect Unit)
opts =
  subparser
    ( command "deps" (info (pure installDeps) (progDesc "Install dependencies"))
    )
