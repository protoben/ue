module Data.Units.Types where

import Data.List

data BaseDimension = Mass | Distance | Luminosity | Time | Temperature | Current
    deriving (Show, Eq, Ord)

-- a dimension defined in terms of (t1*t2*t3...*tN)/(b1*b2*b3...*bN)
data Dimension = Dimension [BaseDimension] [BaseDimension] | Dimensionless deriving Show

instance Eq Dimension where
    (==) Dimensionless Dimensionless = True
    (==) (Dimension _ _) Dimensionless = False
    (==) Dimensionless (Dimension _ _) = False
    (==) a b = let
        (Dimension ts bs,Dimension ts' bs') = (reduceDim a, reduceDim b) in
            (sort ts == sort ts') && (sort bs == sort bs')

reduceDim :: Dimension -> Dimension
reduceDim Dimensionless = Dimensionless
reduceDim (Dimension ts bs) = reducer (sort ts) (sort bs) where
    reducer [] ys = Dimension [] ys
    reducer (x:xs) ys = if elem x ys
        then reducer xs (delete x ys)
        else let (Dimension ts bs) = reducer xs ys in Dimension (x:ts) bs

-- basic unit types
type Abbrev = String
type Fullname = String
data BaseUnit = BaseUnit Abbrev Fullname BaseDimension deriving Show
data DerivedUnit = DerivedUnit Abbrev Fullname
    [(Rational,BaseUnit)] [(Rational,BaseUnit)] deriving Show

class Dimensioned a where
    dimension :: a -> Dimension

instance Dimensioned BaseUnit where
    dimension (BaseUnit _ _ d) = Dimension [d] []

instance Dimensioned DerivedUnit where
    dimension (DerivedUnit _ _ ts bs) =
        reduceDim $ Dimension (tts ++ bbs) (tbs ++ bts) where
            onBoth :: (a -> b) -> (a,a) -> (b,b)
            onBoth f (x,y) = (f x, f y)
            dimensions :: [(Rational,BaseUnit)] -> [Dimension]
            dimensions = filter (\x->not $ x == Dimensionless) . map (dimension . snd)
            (tts,tbs) = onBoth concat $ unzip $ map (\(Dimension t b)->(t,b)) $ dimensions ts
            (bts,bbs) = onBoth concat $ unzip $ map (\(Dimension t b)->(t,b)) $ dimensions bs

checkDims :: (Dimensioned a, Dimensioned b) => a -> b -> Bool
checkDims a b = (dimension a) == (dimension b)

multiplyDims :: Dimension -> Dimension -> Dimension
multiplyDims (Dimension ts bs) (Dimension ts' bs') = reduceDim $
    Dimension (ts++ts') (bs++bs')

-- a consistent system of units
data UnitSystem = UnitSystem {
    baseUnits :: [BaseUnit],
    derivedUnits :: [DerivedUnit] } deriving Show
