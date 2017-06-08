module Text.REPL (REPLCommand(..), EvalMode(..), runREPL) where

import Data.Units
import Data.List
import Data.Display

import Text.Expression
import Data.Expression

import Math.REPL
import Math.Rewrite
import Math.Approximate
import qualified Math.Environment as E

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
        inSpace (char '_') >> (lift lastResult) >>= (\x->case x of
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

substResult :: Monad m => Expr -> ReplT m Expr
substResult e = substFuncs e >>= substVars >>= (saveResult . simplify)

number :: Num n => (n -> a -> b) -> [a] -> [b]
number f = snd . mapAccumL (\i e->(i+1, f i e)) 1

-- evaluate an expression in the REPL using a specific mode
evaluateCmd :: Monad m => (TextOut -> ReplT m ()) -> EvalMode -> Expr -> ReplT m ()
evaluateCmd write Normal e = evaluateCmd write Symbolic e
evaluateCmd write Symbolic e = substResult e >>= write . display
evaluateCmd write DebugReductions e = do
    a <- substFuncs e
    b <- substVars a
    let history = a:b:simplifyHistory b
    let lines = number (\x e->write $ [(Prompt,show x ++ ". "),
                                         (Variable,show e)]) history
    sequence_ lines
evaluateCmd write ShowReductions e = do
    a <- substFuncs e
    b <- substVars a
    let history = a:b:simplifyHistory b
    let lines = number (\x e->write $ (Prompt,show x ++ ". "):display e) history
    sequence_ lines
evaluateCmd write AvoidExpansion e = saveResult (simplify e) >>= write . display
evaluateCmd write Verbatim e = saveResult e >>= write . display
evaluateCmd write Approximate e = substFuncs e >>= substVars >>=
    (\e->case approx (simplify e) of
        Left (UnitsError u v)  -> write $ concat [
                                  [(ErrorMsg, "Incompatible units: ")],
                                  display u, [(ErrorMsg, " vs ")], display v]
        Left (UnitPowerError u)-> write [(ErrorMsg,
                                    "Value with units cannot be exponents")]
        Left TypeError         -> write [(ErrorMsg, "Type error")]
        Left NotConcrete       -> write [(ErrorMsg,
                                  "Expression is not concrete")]
        Right e                -> saveResult e >>= write . display)
evaluateCmd write TypeQuery e = substFuncs e >>= substVars >>=
    (\e->case approx (simplify e) of
        Left (UnitsError u v)  -> write $ concat [
                                  [(ErrorMsg, "Incompatible units: ")],
                                  display u, [(ErrorMsg, " vs ")], display v]
        Left (UnitPowerError u)-> write [(ErrorMsg,
                                    "Value with units cannot be exponents")]
        Left NotConcrete       -> write [(ErrorMsg,
                                  "Expression is not concrete")]
        Right (Constant v)     -> write [(Prompt, show $ dimension v)]
        Right e                -> write [(ErrorMsg,
                                     "Cannot get type of non-value expression")])
evaluateCmd write DebugDump e = write [(Prompt, show e)]
evaluateCmd write ResultDump e = substFuncs e >>= substVars >>=
                                 (\x->write [(Prompt, show $ simplify x)])

-- execute a REPL command
executeCmd :: Monad m => (TextOut -> ReplT m ()) -> REPLCommand -> ReplT m ()
executeCmd write cmd = case cmd of
    (Evaluate m e)    -> evaluateCmd write m e
    (VarBind v e)     -> bindVar v e
    (FuncBind n as e) -> bindFunc n $ E.SymbolicFn as e
    (VarUnbind n)     -> unbindVar n
    (FuncUnbind n)    -> unbindFunc n
    Help              -> mapM_ (write . (\l->[(Prompt,l)])) [
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

runREPL :: Monad m => ReplT m String -> (TextOut -> ReplT m ()) -> m ()
runREPL read write = runReplT $ withBindings [] [] $ forever $ do
    cmd <- read >>= runParserT replCommand () "<repl>"
    case cmd of
        Left e  -> write [(ErrorMsg, show e)]
        Right c -> executeCmd write c
