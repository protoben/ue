module Data.Units where

import Data.List

data BaseDimension = Mass | Distance | Luminosity | Time | Temperature | Energy
    deriving (Show, Eq, Ord)

-- a dimension defined in terms of (t1*t2*t3...*tN)/(b1*b2*b3...*bN)
data Dimension = Dimension [BaseDimension] [BaseDimension] deriving Show

instance Eq Dimension where
    (==) a b = let
        (Dimension ts bs,Dimension ts' bs') = (reduceDim a, reduceDim b) in
            (sort ts == sort ts') && (sort bs == sort bs')

reduceDim :: Dimension -> Dimension
reduceDim (Dimension ts bs) = reducer (sort ts) (sort bs) where
    reducer [] ys = Dimension [] ys
    reducer (x:xs) ys = if elem x ys
        then reducer xs (delete x ys)
        else let (Dimension ts bs) = reducer xs ys in Dimension (x:ts) bs
