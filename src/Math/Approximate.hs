module Math.Approximate (approx, ApproxError(..)) where

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
exactFracPower (a,ae,u) (b,be) = ExactReal 0 0 u
-- TODO: Implement this properly

-- TODO: Implement conversions between different units with matching dimensions
approxBinary :: BinOp -> Value -> Value -> ApproxResult
approxBinary Add      l@(IntValue a u) r@(IntValue b u') = if isCompat l r
    then Right $ IntValue (a + b) u
    else Left $ UnitsError u u'
approxBinary Subtract l@(IntValue a u) r@(IntValue b u') = if isCompat l r
    then Right $ IntValue (a - b) u
    else Left $ UnitsError u u'
approxBinary Multiply (IntValue a u) (IntValue b u') =
    Right $ IntValue (a * b) (u >* u')
approxBinary Divide   (IntValue a u) (IntValue b u') = if (mod a b) == 0
    then Right $ IntValue (div a b) (u >/ u')
    else approxBinary Divide (ExactReal a 0 u) (ExactReal b 0 u')
approxBinary Power  l@(IntValue a u) r@(IntValue b u') =
    if (dimension r) == Dimensionless then Right $ IntValue (a ^ b) u
                                      else Left $ UnitPowerError u'
approxBinary Add    l@(ExactReal a ae u) r@(ExactReal b be u') = if isCompat l r
    then case compare ae be of
        EQ -> Right $ ExactReal (a+b) ae u
        LT -> approxBinary Add (ExactReal a ae u) (ExactReal (b*(be-ae)) ae u')
        GT -> approxBinary Add (ExactReal (a*(ae-be)) be u) (ExactReal b be u')
    else Left $ UnitsError u u'
approxBinary Subtract (ExactReal a ae u) (ExactReal b be u') = if isCompat u u'
    then case compare ae be of
        EQ -> Right $ ExactReal (a-b) ae u
        LT -> approxBinary Add (ExactReal a ae u) (ExactReal (b*(be-ae)) ae u')
        GT -> approxBinary Add (ExactReal (a*(ae-be)) be u) (ExactReal b be u')
    else Left $ UnitsError u u'
approxBinary Multiply (ExactReal a ae u) (ExactReal b be u') = Right $
    ExactReal (a*b) (ae+be) (u >* u')
approxBinary Divide   (ExactReal a ae u) (ExactReal b be u') = Right $
    ExactReal (a*(10^30) `div` b) (ae-be-30) (u >/ u') -- fix precision here
approxBinary Power    l@(ExactReal a ae u) r@(ExactReal b be u') =
    if dimension u' /= Dimensionless then
        approxBinary Multiply
            (exactIntPower (a,ae,u) $ ipartExact (b,be))
            (exactFracPower (a,ae,u) $ fpartExact (b,be))
    else Left $ UnitPowerError u'
approxBinary o (IntValue n u) e@(ExactReal _ _ _) =
    approxBinary o (ExactReal n 0 u) e

approx :: Expr -> Either ApproxError Expr
approx (RelationExpr op a b) = case (approx a, approx b) of
    (Right l, Right r) -> Right $ RelationExpr op l r
    (Left _,  Right r) -> Right $ RelationExpr op a r
    (Right l, Left _)  -> Right $ RelationExpr op l b
    (Left le, Left re) -> Left le
approx e = if containsSymbols e then Left NotConcrete
                                else Constant <$> approx' e where
    approx' :: Expr -> ApproxResult
    approx' (BinaryExpr op a b) =
        approx' a >>= (\a->approx' b >>= (approxBinary op a))
    approx' (UnaryExpr Negate a) = flipSign <$> approx' a
    approx' (Constant v) = Right v
