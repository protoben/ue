module Math.Functions where

import Data.Expression

data Function =
    Symbolic [String] Expr |
    Approximate [String] ([Value] -> Maybe Value)

