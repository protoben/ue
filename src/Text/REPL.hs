module Text.REPL (REPLCommand(..), EvalMode(..), replCommand, readRepl) where

import Text.Expression
import Data.Expression

import Text.Parsec
import Text.Parsec.String

import Control.Monad

data EvalMode = Numeric | Symbolic | AvoidExpansion | Verbatim
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

-- main parser
replCommand :: Monad m => CParserT m REPLCommand
replCommand = spaces >> (choice $ map try [
    char ',' >> spaces >> liftM (Evaluate DebugDump) expr,
    char ';' >> spaces >> liftM (Evaluate ResultDump) expr,
    char '!' >> spaces >> liftM (Evaluate Numeric) expr,
    char '"' >> spaces >> liftM (Evaluate Verbatim) expr,
    char '`' >> spaces >> liftM (Evaluate AvoidExpansion) expr,
    char '\\' >> spaces >> liftM (Evaluate ShowReductions) expr,
    string "\\\\" >> spaces >> liftM (Evaluate DebugReductions) expr,
    char '\'' >> spaces >> liftM (Evaluate Symbolic) expr,
    string "!?" >> spaces >> liftM (Evaluate TypeQuery) expr,
    liftM2 VarBind (inSpace name) $ symbol ":=" >> expr,
    funcBind,
    ((void $ char '?') <|> symbol "help") >> return Help,
    liftM (Evaluate Normal) expr,
    return NoAction
    ]) >>= (\x->spaces >> eof >> return x)

readRepl :: IO (Either ParseError REPLCommand)
readRepl = liftM (parse replCommand "<user>") getLine
