module Main where

import Text.Parsec

import Data.List
import Data.Expression
import Text.Expression
import Text.REPL

import Math.REPL
import Math.Rewrite
import Math.Approximate

import Data.Units

import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.IO.Class

parseExpr :: String -> Either ParseError Expr
parseExpr = parse (expr >>= (\x->eof >> return x)) "cmd"

printExpr :: (MonadIO m) => Expr -> m ()
printExpr = liftIO . putStrLn . display

replDo :: REPLCommand -> ReplT IO ()
replDo (Evaluate Normal e) = substFuncs e >>= substVars >>= (printExpr . simplify)
replDo (Evaluate Symbolic e) = substFuncs e >>= substVars >>= (printExpr.simplify)
replDo (Evaluate ShowReductions e) = do
    a <- substFuncs e
    b <- substVars a
    let history = mapAccumL (\i e->(i+1, (show i) ++ ". " ++ (display e))) 1
            (a:b:reductions b)
    liftIO $ mapM_ putStrLn $ snd history
replDo (Evaluate AvoidExpansion e) = printExpr $ simplify e
replDo (Evaluate Verbatim e) = printExpr e
replDo (Evaluate Numeric e) = liftM simplify (substFuncs e >>= substVars) >>=
    (\e->liftIO $ if containsSymbols e
         then putStrLn "Failed: expression is not concrete"
         else case (approx e) of
            Left (UnitsError u v) -> putStrLn $ concat ["Incompatible units: ",
                displayUnit u, " vs ", displayUnit v]
            Left err -> print err
            Right v  -> liftIO $ putStrLn $ display $ Constant v)
replDo (VarBind n e) = bindVar n e
replDo Help = return ()
replDo NoAction = return ()
replDo _    = liftIO $ putStrLn "Unknown REPL command"

replOnce :: ReplT IO ()
replOnce = do
    parseResult <- liftIO readRepl
    case parseResult of
        Left e -> liftIO $ print e
        Right c -> replDo c

main :: IO ()
main = runReplT $ withBindings [] [] $ forever replOnce
