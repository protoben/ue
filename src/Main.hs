module Main where

import Data.Expression
import Text.Expression
import Text.Parsec
import Math.Rewrite
import Math.Environment

import Control.Monad
import Control.Monad.Trans.Class

parseExpr :: String -> Either ParseError Expr
parseExpr = parse (expr >>= (\x->eof >> return x)) "cmd"

replOnce :: EnvT IO ()
replOnce = do
    ln <- lift getLine
    let (debugging, parseResult) = if take 1 ln == ":"
        then (True, parseExpr (tail ln))
        else (False, parseExpr ln)

    if debugging
    then (either (\x->lift $ print x) (\x->do
        s <- substFuncs x >>= substVars
        lift $ mapM_ putStrLn $ map display $ reductions s) parseResult)
    else (either (\x->lift $ print x) (\x->do
        s <- substFuncs x >>= substVars
        lift $ putStrLn $ display $ simplify s) parseResult)

main :: IO ()
main = evalEnvT emptyEnv $ withBindings [] [] $
    forever replOnce
