module Main where

import Text.Parsec

import Data.List
import Data.Expression
import Text.Expression
import Text.REPL
import Data.Display

import Math.REPL
import Math.Rewrite
import Math.Approximate
import qualified Math.Environment as E

import Data.Units

import UI.REPL

import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.IO.Class

parseExpr :: String -> Either ParseError Expr
parseExpr = parse (expr >>= (\x->eof >> return x)) "cmd"

printExpr :: (MonadIO m) => Expr -> m ()
printExpr = liftIO . putStrLn . displayColors

substResult :: Monad m => Expr -> ReplT m Expr
substResult e = substFuncs e >>= substVars >>= (saveResult . simplify)

replDo :: REPLCommand -> ReplT IO ()
replDo (Evaluate Normal e) = substResult e >>= printExpr
replDo (Evaluate Symbolic e) = substResult e >>= printExpr
replDo (Evaluate DebugReductions e) = do
    a <- substFuncs e
    b <- substVars a
    let history = mapAccumL (\i e->(i+1, (show i) ++ ". " ++ (show e))) 1
            (a:b:simplifyHistory b)
    liftIO $ mapM_ putStrLn $ snd history
replDo (Evaluate ShowReductions e) = do
    a <- substFuncs e
    b <- substVars a
    let history = mapAccumL (\i e->(i+1, (show i) ++ ". " ++ (displayColors e))) 1
            (a:b:simplifyHistory b)
    liftIO $ mapM_ putStrLn $ snd history
replDo (Evaluate AvoidExpansion e) = (saveResult $ simplify e) >>= printExpr
replDo (Evaluate Verbatim e) = saveResult e >>= printExpr
replDo (Evaluate Approximate e) =
    liftM simplify (substFuncs e >>= substVars) >>= (\e->case (approx e) of
        Left (UnitsError u v) -> liftIO $ putStrLn $
            concat ["Incompatible units: ",displayUnit u, " vs ", displayUnit v]
        Left NotConcrete -> liftIO $ putStrLn "Failed: expression is not concrete"
        Left err -> liftIO $ print err
        Right v  -> saveResult v >>= (liftIO . putStrLn . displayColors))
replDo (Evaluate TypeQuery e) = liftM simplify (substFuncs e >>= substVars) >>=
    (\e->liftIO $ if containsSymbols e
         then putStrLn "Failed: expression is not concrete"
         else case (approx e) of
            Left (UnitsError u v) -> putStrLn $ concat ["Incompatible units: ",
                displayUnit u, " vs ", displayUnit v]
            Left err -> print err
            Right (Constant v) -> print $ dimension v
            Right _ -> putStrLn "Cannot get type of non-value expression")
replDo (Evaluate DebugDump e) = liftIO $ print e
replDo (Evaluate ResultDump e) = substFuncs e >>= substVars >>=
    (liftIO . print . simplify)
replDo (VarBind n e) = bindVar n e
replDo (FuncBind n a e) = bindFunc n $ E.SymbolicFn a e
replDo Help = liftIO $ mapM_ putStrLn [
    "ue - universal evaluator - v0.1",
    "      REPL COMMAND LIST",
    "<expr>                 Evaluate an expression",
    ",<expr>                Show the internal AST for an expression",
    ";<expr>                Evaluate an expression and show its AST",
    "!<expr>                Approximate an expression numerically",
    "\"<expr>                Echo an expression without evaluation",
    "`<expr>                Evaluate an expression without expanding variables",
    "                       or function applications.",
    "\\<expr>                Evaluate, showing each reduction step",
    "\\\\<expr>               Evaluate, showing the AST at each reduction",
    --"'<expr>                Evaluate in symbolic mode",
    "!?<expr>               Show the dimensions of an expression",
    "v := e                 Define variable v as expression e",
    "f(x,y..) := e          Define function f as expression e",
    "?                      Display this help",
    "help                   Display this help"]
replDo NoAction = return ()
replDo _    = liftIO $ putStrLn "Unknown REPL command"

replOnce :: ReplT IO ()
replOnce = do
    parseResult <- readRepl
    case parseResult of
        Left e -> liftIO $ print e
        Right c -> replDo c

main :: IO ()
main = runReplT $ withBindings [] [] $ forever replOnce
