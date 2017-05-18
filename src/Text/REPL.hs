module Text.REPL (REPLCommand(..), EvalMode(..), replCommand, readRepl) where

import Text.Expression
import Data.Expression

import Text.Parsec
import Text.Parsec.String

import Control.Monad

data EvalMode = Numeric | Symbolic | AvoidExpansion | Verbatim
    | Normal | ShowReductions deriving Show

data REPLCommand = VarBind String Expr |
    FuncBind String [String] Expr |
    VarUnbind String | FuncUnbind String |
    Help | Evaluate EvalMode Expr | NoAction deriving Show

-- utility functions
inSpace :: Parser a -> Parser a
inSpace p = spaces >> p >>= (\r->spaces >> return r)

symbol :: String -> Parser ()
symbol s = (inSpace $ string s) >> return ()

name :: Parser String
name = inSpace $ liftM2 (:) (letter <|> char '_') (many $ alphaNum <|> char '_')

-- internal parsers
funcBind :: Parser REPLCommand
funcBind = char '^' >> return Help

-- main parser
replCommand :: Parser REPLCommand
replCommand = spaces >> (choice $ map try [
    char '!' >> liftM (Evaluate Numeric) expr,
    char '"' >> liftM (Evaluate Verbatim) expr,
    char '`' >> liftM (Evaluate AvoidExpansion) expr,
    char '\\' >> liftM (Evaluate ShowReductions) expr,
    char '\'' >> liftM (Evaluate Symbolic) expr,
    liftM2 VarBind (inSpace name) $ symbol ":=" >> expr,
    funcBind,
    ((void $ char '?') <|> symbol "help") >> return Help,
    liftM (Evaluate Normal) expr,
    return NoAction
    ]) >>= (\x->spaces >> eof >> return x)

readRepl :: IO (Either ParseError REPLCommand)
readRepl = liftM (parse replCommand "<user>") getLine
