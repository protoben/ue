module UI.REPL (colorize) where

import Data.List
import Data.Display

colorize' :: (ContentClass, String) -> String
colorize' (Variable, s) = "\x1b[0m" ++ s
colorize' (Units, s)    = "\x1b[31m" ++ s
colorize' (Numeric, s)  = "\x1b[32m" ++ s
colorize' (Symbol, s)   = "\x1b[33m" ++ s
colorize' (Name, s)     = "\x1b[35m" ++ s
colorize' (ErrorMsg, s) = "\x1b[31;1m" ++ s
colorize' (Prompt, s)   = "\x1b[32;1m" ++ s

colorize :: TextOut -> String
colorize xs = concatMap colorize' xs ++ "\x1b[0m"
