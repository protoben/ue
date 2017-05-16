{-# LANGUAGE TemplateHaskell #-}
module Math.Environment where

import Data.Expression
import Control.Monad.Trans.State.Strict
import Control.Monad
import Control.Lens hiding (element)

type MFunction = ([String], Expr) -- arg names, expression

data Environment = Environment {
    _funcs :: [(String, MFunction)], -- name, function
    _vars  :: [(String, Expr)] -- global variable bindings
    }
makeLenses ''Environment

type EnvM = State Environment
type EnvT m = StateT Environment m

getFuncs :: (Monad m) => EnvT m [(String, MFunction)]
getFuncs = liftM (view funcs) get

getVars :: (Monad m) => EnvT m [(String, Expr)]
getVars = liftM (view vars) get

substFuncs :: (Monad m) => Expr -> EnvT m Expr
substFuncs e = getFuncs >>= (\fs->return $ subst (sub fs) e)
    where
        sub :: [(String, MFunction)] -> Expr -> Maybe Expr
        sub fs (FuncCall n args) = (n `lookup` fs) >>= (bind args)
        sub _ _ = Nothing

        bind :: [Expr] -> ([String], Expr) -> Maybe Expr
        bind [] ((n:ns),expr) = Nothing -- arg count mismatch
        bind (a:as) ([],expr) = Nothing
        bind [] ([],expr) = Just expr
        bind (x:xs) ((n:ns),expr) = substVar n x <$> bind xs (ns,expr)

substVars :: (Monad m) => Expr -> EnvT m Expr
substVars e = getVars >>= (\vs->return $ foldl (\e (v,t)->substVar v t e) e vs)

bindFunc :: (Monad m) => String -> [String] -> Expr -> EnvT m ()
bindFunc n a e = modify' $ over funcs ((n,(a,e)):)

bindVar :: (Monad m) => String -> Expr -> EnvT m ()
bindVar n e = modify' $ over vars ((n,e):)

evalEnvT :: (Monad m) => Environment -> EnvT m a -> m a
evalEnvT e m = evalStateT m e

emptyEnv :: Environment
emptyEnv = Environment [] []

withBindings :: (Monad m) => [(String, Expr)] -> [(String, MFunction)] -> EnvT m a -> EnvT m a
withBindings vs fs = withStateT (\e->(e & vars %~ (vs ++)) & funcs %~ (fs ++))
