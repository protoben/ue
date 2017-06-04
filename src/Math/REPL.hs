{-# LANGUAGE TemplateHaskell #-}
module Math.REPL where

import qualified Math.Environment as E
import Math.Functions
import Data.Expression

import Control.Monad.Trans.State.Strict
import Control.Monad.Trans.Class
import Control.Monad
import Control.Lens hiding (element, Context)

data Options = Options {
    -- whether to round intermediate results when approximating
    _preservePrecision :: Bool,

    -- the number of decimal places of precision to evaluate expressions to
    _precision :: Int
    }
makeLensesFor [
    ("_preservePrecision", "optPreserve"),
    ("_precision", "optPrecision")] ''Options

data Context = Context {
    _options :: Options,
    _previous :: Maybe Expr
    }
makeLenses ''Context

type ReplM = StateT Context E.EnvM
type ReplT m = StateT Context (E.EnvT m)

defaultContext :: Context
defaultContext = Context {
    _options=Options {_preservePrecision=False, _precision=25},
    _previous=Nothing}

preserve :: Monad m => Bool -> ReplT m ()
preserve p = modify' $ set (options . optPreserve) p

precision :: (Monad m) => Int -> ReplT m ()
precision n = modify' $ set (options . optPrecision) n

approximate :: (Monad m) => Expr -> ReplT m (Maybe Expr)
approximate e = return Nothing

runReplT :: (Monad m) => ReplT m a -> m a
runReplT m = E.evalEnvT E.emptyEnv $ do
    setupEnvironment
    evalStateT m defaultContext

-- below here are just operations which re-export Env operations in Repl

substFuncs :: Monad m => Expr -> ReplT m Expr
substFuncs = lift . E.substFuncs

substVars :: Monad m => Expr -> ReplT m Expr
substVars = lift . E.substVars

bindVar :: Monad m => String -> Expr -> ReplT m ()
bindVar s e = lift $ E.bindVar s e

bindFunc :: Monad m => String -> E.Function -> ReplT m ()
bindFunc s f = lift $ E.bindFunc s f

saveResult :: Monad m => Expr -> ReplT m Expr
saveResult e = modify' (set previous (Just e)) >> return e

lastResult :: Monad m => ReplT m (Maybe Expr)
lastResult = liftM (view $ previous) get

withBindings :: (Monad m) => [(String, Expr)] -> [(String, E.Function)] -> ReplT m a -> ReplT m a
withBindings v f m = do
    ctx <- get
    -- run the argument with modified bindings
    (e,nc) <- lift $ E.withBindings v f $ runStateT m ctx
    put nc -- commit changes to the state
    return e
