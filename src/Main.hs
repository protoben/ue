module Main where

import Data.Display

import Math.REPL
import Text.REPL
import UI.REPL

import Control.Monad
import Control.Monad.Trans.Class as T
import Control.Monad.IO.Class

import System.Console.Haskeline
import System.Console.Haskeline.Completion

prompt :: TextOut
prompt = [(Prompt, "$ ")]

settings = setComplete noCompletion defaultSettings

main :: IO ()
main = runInputT settings $ runREPL readline writeline where
    writeline :: TextOut -> ReplT (InputT IO) ()
    writeline = liftRepl . outputStrLn . colorize

    readline :: ReplT (InputT IO) String
    readline = liftRepl (getInputLine $ colorize prompt) >>= (\e->case e of
        Nothing -> readline
        Just l  -> return l)
