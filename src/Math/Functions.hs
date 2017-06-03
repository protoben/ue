module Math.Functions where

import Data.Expression
import Data.Units
import Control.Applicative
import Math.Environment

-- try differentiating with respect to a given variable
differentiate :: Expr -> String -> Maybe Expr
differentiate (RelationExpr _ _ _) x = Nothing
differentiate (BinaryExpr Add a b) x =
    (BinaryExpr Add) <$> (differentiate a x) <*> (differentiate b x)
differentiate (BinaryExpr Subtract a b) x =
    (BinaryExpr Subtract) <$> (differentiate a x) <*> (differentiate b x)
differentiate (BinaryExpr Multiply a b) x =
    case (containsSymbols a, containsSymbols b) of
        (True, True)  -> Just $ Constant $ IntValue 0 noUnit
        (True, False) -> (BinaryExpr Multiply a) <$> differentiate b x
        (False, True) -> (BinaryExpr Multiply b) <$> differentiate a x
        -- product rule
        (False,False) -> let a' = differentiate a x; b' = differentiate b x in
            BinaryExpr Add <$>
                (BinaryExpr Multiply b <$> a') <*>
                (BinaryExpr Multiply a <$> b')
differentiate (BinaryExpr Divide f g) x =
    let f' = differentiate f x; g' = differentiate g x in BinaryExpr Divide <$>
            (BinaryExpr Subtract <$>
                (BinaryExpr Multiply g <$> f') <*>
                (BinaryExpr Multiply f <$> g')) <*>
            (Just $ BinaryExpr Power g (Constant $ IntValue 2 noUnit))
differentiate (NameRef n) x = if x == n then Just $ Constant $ IntValue 1 noUnit
                                        else Just $ Constant $ IntValue 0 noUnit
differentiate (Constant v) x = Just $ Constant $ IntValue 0 noUnit

diff = BuiltinSymbolic $ \xs->case xs of
    -- try to infer symbols for singular exprs
    [e] -> let syms = allSymbols e in (case syms of
        []  -> return $ differentiate e "x"
        [x] -> return $ differentiate e x
        _   -> return Nothing)
    [e,(NameRef v)] -> return $ differentiate e v
    _   -> return Nothing

setupEnvironment :: Monad m => EnvT m ()
setupEnvironment = do
    bindFunc "diff" diff
