module Math.Functions where

import Data.Expression
import Math.Environment

import Math.Functions.Derivative

setupEnvironment :: Monad m => EnvT m ()
setupEnvironment = do
    bindFunc "diff" diff
