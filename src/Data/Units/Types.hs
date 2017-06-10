module Data.Units.Types (
    BaseDimension(..), Dimension(..), BaseUnit(..), DerivedUnit(..),
    AnonymousUnit(..),
    Unit(..), Dimensioned(..), UnitSystem(..),
    isCompat, inv, unitPow, reduceUnit, convertUnit, noUnit,
    msIntersect,
    (>*), (>/)
    ) where

import Data.List
import Data.Ratio
import Data.Function

import Control.Arrow
import Control.Monad

data BaseDimension = Mass | Distance |
                     Luminosity | Time |
                     Temperature | Current deriving (Show, Eq, Ord)

-- a dimension defined in terms of (t1*t2*t3...*tN)/(b1*b2*b3...*bN)
data Dimension = Dimension [BaseDimension] [BaseDimension] | Dimensionless deriving Show

instance Eq Dimension where
    (==) Dimensionless Dimensionless = True
    (==) (Dimension _ _) Dimensionless = False
    (==) Dimensionless (Dimension _ _) = False
    (==) a b = let
        (Dimension ts bs,Dimension ts' bs') = (reduceDim a, reduceDim b) in
            (sort ts == sort ts') && (sort bs == sort bs')

-- |Reduce the complexity of a passed Dimension by cancelling base units
reduceDim :: Dimension -> Dimension
reduceDim Dimensionless = Dimensionless
reduceDim (Dimension ts bs) = reducer (sort ts) (sort bs) where
    reducer [] [] = Dimensionless
    reducer [] ys = Dimension [] ys
    reducer (x:xs) ys = if elem x ys then reducer xs (delete x ys)
                                     else case reducer xs ys of
                                        (Dimension ts bs) -> Dimension (x:ts) bs
                                        Dimensionless     -> Dimension [x] []

-- basic unit types
type Abbrev = String
type Fullname = String
data BaseUnit = BaseUnit Abbrev Fullname BaseDimension deriving (Show,Eq,Ord)
data DerivedUnit = DerivedUnit Abbrev Fullname
    [(Rational,BaseUnit)] [(Rational,BaseUnit)] deriving (Eq,Show)

-- |Typeclass for everything that has a dimension
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
            dimensions = filter (\x->not $ x == Dimensionless) .
                map (dimension . snd)
            joinFrac = onBoth concat . unzip . map (\(Dimension t b)->(t,b))
            (tts,tbs) = joinFrac $ dimensions ts
            (bts,bbs) = joinFrac $ dimensions bs

isCompat :: (Dimensioned a, Dimensioned b) => a -> b -> Bool
isCompat a b = (dimension a) == (dimension b)

multiplyDims :: Dimension -> Dimension -> Dimension
multiplyDims (Dimension ts bs) (Dimension ts' bs') = reduceDim $
    Dimension (ts++ts') (bs++bs')

-- a consistent system of units
data UnitSystem = UnitSystem {
    baseUnits :: [BaseUnit],
    atomicUnits :: [DerivedUnit], -- units that can't form compound units
    derivedUnits :: [DerivedUnit] } deriving Show

-- anonymous unit type used when evaluating expressions
newtype AnonymousUnit = AnonymousUnit
    ([(Rational,BaseUnit)],[(Rational,BaseUnit)]) deriving (Show,Eq)

class Unit a where
    toFrac :: a -> AnonymousUnit

instance Unit BaseUnit where
    toFrac x = AnonymousUnit ([(1,x)],[])

instance Unit DerivedUnit where
    toFrac (DerivedUnit _ _ ts bs) = AnonymousUnit (ts,bs)

instance Unit AnonymousUnit where
    toFrac x = x

(>*) :: (Unit a, Unit b) => a -> b -> AnonymousUnit
(>*) a b = reduceUnitSimple $ AnonymousUnit (aTop ++ bTop, aBot ++ bBot) where
    (AnonymousUnit (aTop,aBot)) = toFrac a
    (AnonymousUnit (bTop,bBot)) = toFrac b

(>/) :: (Unit a, Unit b) => a -> b -> AnonymousUnit
(>/) a b = reduceUnitSimple $ AnonymousUnit (aTop ++ bBot, aBot ++ bTop) where
    (AnonymousUnit (aTop,aBot)) = toFrac a
    (AnonymousUnit (bTop,bBot)) = toFrac b

inv :: (Unit a) => a -> AnonymousUnit
inv = (\(AnonymousUnit (a,b))->(AnonymousUnit (b,a))) . toFrac

unitPow :: (Unit a) => Int -> a -> AnonymousUnit
unitPow n u = let (AnonymousUnit (t,b)) = toFrac u in
    reduceUnitSimple $ AnonymousUnit
        (concat $ replicate n t,concat $ replicate n b)

-- Cancel out equivalent dimensions. Note that this doesn't work for units which
-- are scaled versions of one another.
reduceUnitSimple :: Unit a => a -> AnonymousUnit
reduceUnitSimple u = reducer (sortFactors ts) (sortFactors bs) where
    (AnonymousUnit (ts,bs)) = toFrac u

    sortFactors :: [(a,BaseUnit)] -> [(a,BaseUnit)]
    sortFactors = sortBy (compare `on` snd)
    reducer [] ys = AnonymousUnit ([],ys)
    reducer (x:xs) ys = if elem x ys then reducer xs (delete x ys) else
        let (AnonymousUnit (ts,bs)) = reducer xs ys in AnonymousUnit (x:ts,bs)

-- Reduce a unit to its simplest form and return the value multiplier that's
-- left over after reduction. This version works for scaled multiples
reduceUnit :: Unit a => a -> (Rational, AnonymousUnit)
reduceUnit x = (finalFactor, finalUnit) where
    (AnonymousUnit (ts,bs)) = toFrac x
    (ts', bs') = (sort $ map snd ts, sort $ map snd bs)

    -- take the base-10 log of an integer rounded down
    log10 :: Rational -> Integer
    log10 x = (case (numerator x, denominator x) of
        (1, n) -> -(log10' n)
        (n, 1) -> log10' n
        (n, m) -> 0) where
            log10' x = if x `mod` 10 == 0 then 1+log10' (x `div` 10) else 0

    -- split an integer at the edge of a given range and return the inner and
    -- outer parts separately
    splitRange :: (Integer,Integer) -> Integer -> (Integer, Integer)
    splitRange (low, high) x = case (x < low, x > high) of
        (True, False)  -> (low, x-low)
        (False, True)  -> (high, x-high)
        (False, False) -> (x, 0)

    -- remove multiples from everything to build final scale factor. this might
    -- not be a power of 1000, so we have to do more work to make sure it is. If
    -- a unit multiple doesn't have a corresponding derived unit, the system can
    -- get confused
    factor = product (map fst ts) / product (map fst bs)
    factorLog = log10 factor

    -- avoid going above or below the limits of SI prefixes
    thousands = splitRange (-24, 24) $ factorLog - (factorLog `mod` 3)

    siFactor = 10^(fst thousands)
    extFactor = 10^(snd thousands + factorLog `mod` 3)

    finalFactor = if finalUnit /= noUnit then extFactor else factor

    -- compute the final unit. If it's non-empty, multiply the residual factor
    -- into the first component on top. If no top unit exists, multiply it in
    -- on the bottom (after taking the reciprocal).
    finalUnit = case reduced of
        (AnonymousUnit ([],[])) -> noUnit
        (AnonymousUnit ((t:ts),b)) -> let newFirst = (fst t * siFactor, snd t)
                                      in AnonymousUnit ((newFirst:ts), b)
        (AnonymousUnit ([],(b:bs))) -> let newFirst = (fst b / siFactor, snd b)
                                       in AnonymousUnit ([], (newFirst:bs))

    reduced = let (t,b) = reducer ts' bs'; tag = (\x->(1,x))
              in AnonymousUnit (map tag t, map tag b)

    reducer :: [BaseUnit] -> [BaseUnit] -> ([BaseUnit], [BaseUnit])
    reducer [] ys = ([],ys)
    reducer (x:xs) ys = if elem x ys then reducer xs (delete x ys) else
        let (ts,bs) = reducer xs ys in (x:ts,bs)

-- find the conversion factor from one basic unit to another
conversionFactor :: (Rational,BaseUnit) -> (Rational,BaseUnit)
                    -> Maybe Rational
conversionFactor a@(ar,au) b@(br,bu)
    | not (isCompat au bu)  = Nothing
    | au == bu              = Just $ ar / br
-- TODO: handle compatible units from different systems

-- multiset intersection for lists
msIntersect :: Eq a => [a] -> [a] -> [a]
msIntersect [] ys = []
msIntersect xs [] = []
msIntersect (x:xs) ys = if x `elem` ys then x:(msIntersect xs $ delete x ys)
                                       else msIntersect xs ys

-- implement the factor-label method for unit conversion
factorLabel :: AnonymousUnit -> AnonymousUnit -> Maybe Rational
factorLabel (AnonymousUnit ([x],[])) (AnonymousUnit ([y],[])) =
    if isCompat (snd x) (snd y) then conversionFactor x y else Nothing
factorLabel (AnonymousUnit ([],[x])) (AnonymousUnit ([],[y])) =
    if isCompat (snd x) (snd y) then conversionFactor x y else Nothing
factorLabel (AnonymousUnit (xs,ys)) (AnonymousUnit (xs',ys')) = factor where
    common :: ([(Rational,BaseUnit)],[(Rational,BaseUnit)])

    -- split into the common part (that we don't need to convert) and the rest
    -- which needs to be adapted
    common = (xs `msIntersect` xs', ys `msIntersect` ys')
    (restX,restY) = ((xs \\ (fst common), ys \\ (snd common)),
                     (xs' \\ (fst common), ys' \\ (snd common)))

    -- zip dimensionally compatible units from the given lists together and
    -- return the overall conversion factor
    cvtDims  :: [(Rational,BaseUnit)] -> [(Rational,BaseUnit)] -> Maybe Rational
    cvtDims xs ys = if xdims /= ydims then Nothing
        else fmap product $ mapM (uncurry conversionFactor) pairs where
            basedim :: BaseUnit -> BaseDimension
            basedim (BaseUnit _ _ d) = d
            (xs',ys') = (sortOn (basedim . snd) xs, sortOn (basedim . snd) ys)
            (xdims,ydims) = (map (basedim . snd) xs', map (basedim . snd) ys')
            pairs :: [((Rational,BaseUnit),(Rational,BaseUnit))]
            pairs = zip xs' ys'

    -- generate overall non-compatible unit factor from top and bottom factors
    topFactor = cvtDims (fst restX) (fst restY)
    btmFactor = cvtDims (snd restX) (snd restY)
    factor = liftM2 (\t b->t / b) topFactor btmFactor

-- Return the conversion factor from one unit to another of the same dimension.
-- If dimensions don't match, returns Nothing
convertUnit :: (Unit a, Unit b) => a -> b -> Maybe Rational
convertUnit a b = if not $ isCompat aa ab
        then Nothing else factorLabel aa ab where
    -- convert to anonymous unit forms
    aa :: AnonymousUnit
    ab :: AnonymousUnit
    (aa,ab) = (toFrac a, toFrac b)

noUnit :: AnonymousUnit
noUnit = AnonymousUnit ([],[])

-- all units are dimensioned
instance Dimensioned AnonymousUnit where
    dimension (AnonymousUnit ([],[])) = Dimensionless
    dimension (AnonymousUnit (t,b)) = Dimension dt db where
        dimify :: (Rational,BaseUnit) -> BaseDimension
        dimify = (\(BaseUnit _ _ x)->x) . snd
        (dt,db) = (map dimify t, map dimify b)
