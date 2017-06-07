module Main where

import Data.Display

import Math.REPL
import Text.REPL
import UI.REPL

import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.IO.Class

main :: IO ()
main = runREPL readline writeline where
    writeline :: TextOut -> ReplT IO ()
    writeline = liftIO . putStrLn . colorize

    readline :: ReplT IO String
    readline = liftIO getLine
