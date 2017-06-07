module UI.REPL (colorize) where

import Text.Parsec

import Data.Display
import Data.Expression
import Text.Expression
import Text.REPL

import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.IO.Class

colorize' :: TextOut -> String
colorize' [] = ""
colorize' ((Variable, s):xs) = "\x1b[0m" ++ s ++ colorize xs
colorize' ((Units, s):xs)    = "\x1b[31m" ++ s ++ colorize xs
colorize' ((Numeric, s):xs)  = "\x1b[32m" ++ s ++ colorize xs
colorize' ((Symbol, s):xs)   = "\x1b[33m" ++ s ++ colorize xs
colorize' ((Name, s):xs)     = "\x1b[35m" ++ s ++ colorize xs
colorize' ((ErrorMsg, s):xs) = "\x1b[31;1m" ++ s ++ colorize xs
colorize' ((Prompt, s):xs)   = "\x1b[32;1m" ++ s ++ colorize xs

colorize :: TextOut -> String
colorize xs = colorize' xs ++ "\x1b[0m"
