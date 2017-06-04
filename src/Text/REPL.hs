module Text.REPL (REPLCommand(..), EvalMode(..), replCommand, readRepl) where

import Text.Expression
import Data.Expression
import Math.REPL

import Text.Parsec
import Text.Parsec.String

import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.IO.Class

data EvalMode = Approximate | Symbolic | AvoidExpansion | Verbatim
    | Normal | ShowReductions | TypeQuery | DebugDump | ResultDump
    | DebugReductions
    deriving Show

data REPLCommand = VarBind String Expr |
    FuncBind String [String] Expr |
    VarUnbind String | FuncUnbind String |
    Help | Evaluate EvalMode Expr | NoAction deriving Show

type CParserT = ParsecT String ()

-- utility functions
inSpace :: Monad m => CParserT m a -> CParserT m a
inSpace p = spaces >> p >>= (\r->spaces >> return r)

symbol :: Monad m => String -> CParserT m ()
symbol s = (inSpace $ string s) >> return ()

name :: Monad m => CParserT m String
name = inSpace $ liftM2 (:) letter (many $ alphaNum <|> char '_')

-- internal parsers
funcBind :: Monad m => CParserT m REPLCommand
funcBind = char '^' >> return Help

replExpr :: Monad m => CParserT (ReplT m) Expr
replExpr = exprExtended [
        char '_' >> (lift lastResult) >>= (\x->case x of
            Nothing -> fail "No previous result to retrieve"
            Just r -> return r)]

-- main parser
replCommand :: Monad m => CParserT (ReplT m) REPLCommand
replCommand = spaces >> (choice $ map try [
    char ',' >> spaces >> liftM (Evaluate DebugDump) replExpr,
    char ';' >> spaces >> liftM (Evaluate ResultDump) replExpr,
    char '!' >> spaces >> liftM (Evaluate Approximate) replExpr,
    char '"' >> spaces >> liftM (Evaluate Verbatim) replExpr,
    char '`' >> spaces >> liftM (Evaluate AvoidExpansion) replExpr,
    char '\\' >> spaces >> liftM (Evaluate ShowReductions) replExpr,
    string "\\\\" >> spaces >> liftM (Evaluate DebugReductions) replExpr,
    char '\'' >> spaces >> liftM (Evaluate Symbolic) replExpr,
    string "!?" >> spaces >> liftM (Evaluate TypeQuery) replExpr,
    liftM2 VarBind (inSpace name) $ symbol ":=" >> replExpr,
    liftM3 FuncBind name (between (symbol "(") (symbol ")")
        (sepBy1 name $ char ',')) $ symbol ":=" >> replExpr,
    ((void $ char '?') <|> symbol "help") >> return Help,
    liftM (Evaluate Normal) replExpr,
    return NoAction
    ]) >>= (\x->spaces >> eof >> return x)

readRepl :: (MonadIO m) => ReplT m (Either ParseError REPLCommand)
readRepl = do
    l <- liftIO getLine
    runParserT replCommand () "<repl>" l
