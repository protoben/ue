{-# LANGUAGE TemplateHaskell #-}
module Math.Environment where

import Math.Functions
import Data.Expression

import Control.Monad.Trans.State.Strict
import Control.Monad
import Control.Lens hiding (element)

data Environment = Environment {
    _funcs :: [(String, Function)], -- name, function
    _vars  :: [(String, Expr)] -- global variable bindings
    }
makeLenses ''Environment

type EnvM = State Environment
type EnvT m = StateT Environment m

getFuncs :: (Monad m) => EnvT m [(String, Function)]
getFuncs = liftM (view funcs) get

getVars :: (Monad m) => EnvT m [(String, Expr)]
getVars = liftM (view vars) get

substFuncs :: (Monad m) => Expr -> EnvT m Expr
substFuncs e = getFuncs >>= (\fs->return $ subst (sub fs) e)
    where
        sub :: [(String, Function)] -> Expr -> Maybe Expr
        sub fs (FuncCall n args) = (n `lookup` fs) >>= (bind args)
        sub _ _ = Nothing

        -- generate the result of a function given a specific set of arguments
        bind :: [Expr] -> Function -> Maybe Expr
        bind _ (Approximate _ _) = Nothing
        bind [] (Symbolic (n:ns) expr) = Nothing -- arg count mismatch
        bind (a:as) (Symbolic [] expr) = Nothing
        bind [] (Symbolic [] expr) = Just expr
        bind (x:xs) (Symbolic (n:ns) expr) = substVar n x <$>
            bind xs (Symbolic ns expr)

substVars :: (Monad m) => Expr -> EnvT m Expr
substVars e = getVars >>= (\vs->return $ foldl (\e (v,t)->substVar v t e) e vs)

bindFunc :: (Monad m) => String -> Function -> EnvT m ()
bindFunc n f = modify' $ over funcs ((n,f):)

bindVar :: (Monad m) => String -> Expr -> EnvT m ()
bindVar n e = modify' $ over vars ((n,e):)

evalEnvT :: (Monad m) => Environment -> EnvT m a -> m a
evalEnvT e m = evalStateT m e

emptyEnv :: Environment
emptyEnv = Environment [] []

withBindings :: (Monad m) => [(String, Expr)] -> [(String, Function)] -> EnvT m a -> EnvT m a
withBindings vs fs = withStateT (\e->(e & vars %~ (vs ++)) & funcs %~ (fs ++))
