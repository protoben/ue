module Main where

import Data.Expression
import Text.Expression
import Text.Parsec
import Control.Monad
import Math.Rewrite

parseExpr :: String -> Either ParseError Expr
parseExpr = parse (expr >>= (\x->eof >> return x)) "cmd"

replOnce :: IO ()
replOnce = do
    ln <- getLine
    let (debugging, parseResult) = if take 1 ln == ":"
        then (True, parseExpr (tail ln))
        else (False, parseExpr ln)

    if debugging then (either (\x->print x) (\x->mapM_ putStrLn $ map display $ reductions x) parseResult)
        else (either (\x->print x) (\x->print x >> (putStrLn $ display $ simplify x)) parseResult)

main :: IO ()
main = forever replOnce
