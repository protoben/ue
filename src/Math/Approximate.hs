module Math.Approximate where

import Data.Expression
import Math.Functions

import Control.Applicative
import Control.Monad

data ApproxMode =
    AvoidRounding Int |
    RoundTo Int

data ApproxError = TypeError deriving Show

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
flipSign (IntValue a) = IntValue (-a)
flipSign (ExactReal a e) = ExactReal (-a) e
flipSign (Vec2 x y) = Vec2 (flipSign x) (flipSign y)
flipSign (VecN xs) = VecN $ map flipSign xs

-- raise a value to an exact integer power
exactIntPower :: (Integer,Integer) -> Integer -> Value
exactIntPower (n,e) p = ExactReal (n ^ p) (e * p)

-- raise a value to an exact fractional power
exactFracPower :: (Integer,Integer) -> (Integer,Integer) -> Value
exactFracPower (a,ae) (b,be) = ExactReal 0 0
-- TODO: Implement this properly

approxBinary :: BinOp -> Value -> Value -> ApproxResult
approxBinary Add      (IntValue a) (IntValue b) = Right $ IntValue $ a + b
approxBinary Subtract (IntValue a) (IntValue b) = Right $ IntValue $ a - b
approxBinary Multiply (IntValue a) (IntValue b) = Right $ IntValue $ a * b
approxBinary Divide   (IntValue a) (IntValue b) = if (mod a b) == 0
    then Right $ IntValue (div a b)
    else approxBinary Divide (ExactReal a 0) (ExactReal b 0)
approxBinary Power    (IntValue a) (IntValue b) = Right $ IntValue $ a ^ b
approxBinary Add      (ExactReal a ae) (ExactReal b be) = case compare ae be of
        EQ -> Right $ ExactReal (a+b) ae
        LT -> approxBinary Add (ExactReal a ae) (ExactReal (b*(be-ae)) ae)
        GT -> approxBinary Add (ExactReal (a*(ae-be)) be) (ExactReal b be)
approxBinary Subtract (ExactReal a ae) (ExactReal b be) = case compare ae be of
        EQ -> Right $ ExactReal (a-b) ae
        LT -> approxBinary Add (ExactReal a ae) (ExactReal (b*(be-ae)) ae)
        GT -> approxBinary Add (ExactReal (a*(ae-be)) be) (ExactReal b be)
approxBinary Multiply (ExactReal a ae) (ExactReal b be) = Right $
    ExactReal (a*b) (ae+be)
approxBinary Divide   (ExactReal a ae) (ExactReal b be) = Right $
    ExactReal (a*(10^30) `div` b) (ae-be-30) -- fix precision here
approxBinary Power    l@(ExactReal a ae) r@(ExactReal b be) = approxBinary
    Multiply
    (exactIntPower (a,ae) $ ipartExact (b,be))
    (exactFracPower (a,ae) $ fpartExact (b,be))
approxBinary o (IntValue n) e@(ExactReal _ _) = approxBinary o (ExactReal n 0) e

approx :: Expr -> ApproxResult
approx (BinaryExpr op a b) = approx a >>= (\a->approx b >>= (approxBinary op a))
approx (UnaryExpr Negate a) = flipSign <$> approx a
approx (Constant v) = Right v
