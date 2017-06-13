module Math.Approximate (approx, ApproxError(..), compareValues) where

import Data.Expression
import Data.Units
import Math.Functions

import Control.Applicative
import Control.Monad

data ApproxMode =
    AvoidRounding Int |
    RoundTo Int

data ApproxError = TypeError |
    UnitsError AnonymousUnit AnonymousUnit |
    UnitPowerError AnonymousUnit |
    NotConcrete
    deriving Show

type ApproxResult = Either ApproxError Value

-- extract the integer part of an exact value
ipartExact :: (Integer, Integer) -> Integer
ipartExact (n,e) = case compare 0 e of
    LT -> n*(10^e)
    EQ -> n
    GT -> n `div` (10^(-e))

-- extract the fractional part of an exact value
fpartExact :: (Integer, Integer) -> (Integer, Integer)
fpartExact (n,e) = case compare 0 e of
    LT -> (0, 0) -- integer, so no fractional part
    EQ -> (0, 0) -- integer, so no fractional part
    GT -> (n `mod` (10^(-e)), e)

-- flip the sign of the given value
flipSign :: Value -> Value
flipSign (IntValue a u) = IntValue (-a) u
flipSign (ExactReal a e u) = ExactReal (-a) e u
flipSign (Vec2 x y) = Vec2 (flipSign x) (flipSign y)
flipSign (VecN xs) = VecN $ map flipSign xs

-- raise a value to an exact integer power
exactIntPower :: (Integer,Integer,AnonymousUnit) -> Integer -> Value
exactIntPower (n,e,u) p = ExactReal (n ^ p) (e * p) (unitPow (fromIntegral p) u)

-- raise a value to an exact fractional power
exactFracPower :: (Integer,Integer,AnonymousUnit) -> (Integer,Integer) -> Value
exactFracPower (a,ae,u) (b,be) = ExactReal 1 0 u
-- TODO: Implement this properly

-- try to make two values have the same unit
sameUnits :: (Value,Value) -> Either ApproxError (Value,Value)
sameUnits (a,b) = case (valueUnit b) >>= (convertValue a) of
    Nothing -> Left TypeError
    Just r  -> Right (r,b)

approxBinary :: BinOp -> Value -> Value -> ApproxResult
approxBinary o (IntValue n u) e@(ExactReal _ _ _) =
    approxBinary o (ExactReal n 0 u) e
approxBinary o e@(ExactReal _ _ _) (IntValue n u) =
    approxBinary o e (ExactReal n 0 u)
approxBinary Add l r = sameUnits (l,r) >>= (\(l,r)->case (l,r) of
    ((IntValue a u),(IntValue b u')) -> Right $ IntValue (a+b) u
    ((ExactReal a ae u),(ExactReal b be u')) -> case compare ae be of
        EQ -> Right $ ExactReal (a+b) ae u
        LT -> approxBinary Add (ExactReal a ae u)
                               (ExactReal (b*(10^(be-ae))) ae u')
        GT -> approxBinary Add (ExactReal (a*(10^(ae-be))) be u)
                               (ExactReal b be u')
    -- recurse for conversion+normalization
    (a@(IntValue _ _),b@(ExactReal _ _ _)) -> approxBinary Add a b
    (a@(ExactReal _ _ _),b@(IntValue _ _)) -> approxBinary Add a b)
approxBinary Subtract l@(IntValue a u) (IntValue b u') =
    approxBinary Add l (IntValue (-b) u')
approxBinary Multiply (IntValue a u) (IntValue b u') =
    Right $ IntValue (a * b) (u >* u')
approxBinary Divide   (IntValue a u) (IntValue b u') = if (mod a b) == 0
    then Right $ IntValue (div a b) (u >/ u')
    else approxBinary Divide (ExactReal a 0 u) (ExactReal b 0 u')
approxBinary Power l@(IntValue a u) r@(IntValue b u') =
    if (dimension r) == Dimensionless then Right $ IntValue (a ^ b)
                                                            (unitPow (fromIntegral b) u)
                                      else Left $ UnitPowerError u'
approxBinary Subtract (ExactReal a ae u) (ExactReal b be u') =
    approxBinary Add (ExactReal a ae u) (ExactReal (-b) be u')
approxBinary Multiply (ExactReal a ae u) (ExactReal b be u') = Right $
    ExactReal (a*b) (ae+be) (u >* u')
approxBinary Divide   (ExactReal a ae u) (ExactReal b be u') = Right $
    ExactReal (a*(10^30) `div` b) (ae-be-30) (u >/ u') -- fix precision here
approxBinary Power    l@(ExactReal a ae u) r@(ExactReal b be u') =
    if dimension u' == Dimensionless then
        forceUnit (unitPow (fromIntegral $ ipartExact (b,be)) u) <$>
            approxBinary Multiply
                (exactIntPower (a,ae,u) $ ipartExact (b,be))
                (exactFracPower (a,ae,u) $ fpartExact (b,be))
    else Left $ UnitPowerError u'

approx :: Expr -> Either ApproxError Expr
approx (RelationExpr op a b) = case (approx a, approx b) of
    (Right l, Right r) -> Right $ RelationExpr op l r
    (Left _,  Right r) -> Right $ RelationExpr op a r
    (Right l, Left _)  -> Right $ RelationExpr op l b
    (Left le, Left re) -> Left le
approx e = if containsSymbols e then Left NotConcrete
                                else Constant . simplifyVal <$> approx' e where
    simplifyVal :: Value -> Value
    simplifyVal = simpleUnit . reduceVal

    -- try removing decimal places without losing precision
    reduceExact :: Value -> Value
    reduceExact v@(ExactReal m n u) = if m `mod` 10 /= 0 then v
                                      else reduceExact $ ExactReal (m `div` 10)
                                                                   (n + 1) u
    reduceVal :: Value -> Value
    reduceVal v@(IntValue n u) = v
    reduceVal v@(ExactReal m n u) =
        if n >= 0 then IntValue (m*(10^n)) u -- easy case - just multiply
        else (if m `mod` (10^(-n)) == 0 -- check that we can safely convert
            then IntValue (m `div` (10^(-n))) u
            else reduceExact v) -- try reducing decimal precision
    reduceVal v = v

    simpleUnit :: Value -> Value
    simpleUnit v = case valueUnit v of
        Nothing -> v
        Just u  -> case reduceUnit u of
            (1,r) -> forceUnit r v
            (m,r) -> case ratMultiple v m of
                Nothing -> v
                Just rv -> forceUnit r rv

    approx' :: Expr -> ApproxResult
    approx' (BinaryExpr op a b) =
        approx' a >>= (\a->approx' b >>= (approxBinary op a))
    approx' (UnaryExpr Negate a) = flipSign <$> approx' a
    approx' (TypeAssertion e u) = case approx' e of
        Left e  -> Left e
        Right r -> if isCompat u r
            then (case convertValue r u of
                Nothing -> Left TypeError
                Just x  -> Right x)
            else Left TypeError
    approx' (Constant v) = Right v

compareValues :: Value -> Value -> Ordering
compareValues (IntValue n _) (IntValue m _) = compare n m
compareValues e@(ExactReal n p _) (IntValue m _) = compareValues e $ ExactReal m 0 noUnit
compareValues (IntValue m _) e@(ExactReal n p _) = compareValues (ExactReal m 0 noUnit) e
compareValues (ExactReal n p _) (ExactReal m q _) = case compare p q of
    EQ -> compare n m
    LT -> compare (n*(10^(q-p))) m
    GT -> compare n (m*(10^(p-q)))
compareValues _ _ = EQ
