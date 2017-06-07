{-# LANGUAGE TemplateHaskell #-}
module Math.Environment where

import Data.List
import Data.Expression

import Control.Monad.Trans.State.Strict
import Control.Monad.Trans.Class
import Control.Monad
import Control.Lens hiding (element)

type EnvM = State Environment
type EnvT m = StateT Environment m

data Function =
    SymbolicFn [String] Expr |
    BuiltinSymbolic ([Expr] -> EnvM (Maybe Expr)) |
    Approximate [String] ([Value] -> Maybe Value)

data Environment = Environment {
    _funcs :: [(String, Function)], -- name, function
    _vars  :: [(String, Expr)] -- global variable bindings
    }
makeLenses ''Environment

getFuncs :: (Monad m) => EnvT m [(String, Function)]
getFuncs = liftM (view funcs) get

getVars :: (Monad m) => EnvT m [(String, Expr)]
getVars = liftM (view vars) get

substFuncs :: (Monad m) => Expr -> EnvT m Expr
substFuncs e = getFuncs >>= (\fs->substM (sub fs) e)
    where
        -- try applying each function in the list, and return the first one that
        -- works
        sub :: Monad m => [(String, Function)] -> Expr -> EnvT m (Maybe Expr)
        sub fs (FuncCall n args) = (return $ n `lookup` fs) >>= (\x->case x of
            Nothing -> return Nothing
            Just r  -> bind args r)
        sub _ _ = return Nothing

        -- generate the result of a function given a specific set of arguments
        bind :: Monad m => [Expr] -> Function -> EnvT m (Maybe Expr)
        bind xs (BuiltinSymbolic f) = liftEnv $ f xs
        bind _ (Approximate _ _) = return Nothing
        bind [] (SymbolicFn (n:ns) expr) = return Nothing -- arg count mismatch
        bind (a:as) (SymbolicFn [] expr) = return Nothing
        bind [] (SymbolicFn [] expr) = return $ Just expr
        bind (x:xs) (SymbolicFn (n:ns) expr) = liftM (\r->substVar n x <$> r) $
            bind xs (SymbolicFn ns expr)

substVars :: (Monad m) => Expr -> EnvT m Expr
substVars e = getVars >>= (\vs->return $ foldl (\e (v,t)->substVar v t e) e vs)

liftEnv :: (Monad m) => EnvM a -> EnvT m a
liftEnv m = do
    s <- get
    let (r,s') = runState m s
    put s'
    return r

bindFunc :: (Monad m) => String -> Function -> EnvT m ()
bindFunc n f = modify' $ over funcs ((n,f):)

unbindFunc :: Monad m => String -> EnvT m ()
unbindFunc n = modify' $ over funcs (filter (\(a,_)->a /= n))

bindVar :: (Monad m) => String -> Expr -> EnvT m ()
bindVar n e = modify' $ over vars ((n,e):)

unbindVar :: Monad m => String -> EnvT m ()
unbindVar n = modify' $ over vars (filter (\(a,_)->a /= n))

evalEnvT :: (Monad m) => Environment -> EnvT m a -> m a
evalEnvT e m = evalStateT m e

emptyEnv :: Environment
emptyEnv = Environment [] []

withBindings :: (Monad m) => [(String, Expr)] -> [(String, Function)] -> EnvT m a -> EnvT m a
withBindings vs fs = withStateT (\e->(e & vars %~ (vs ++)) & funcs %~ (fs ++))
