{-# LANGUAGE FlexibleInstances #-}
module Data.Units.Definition where

import Data.Units.Types
import Control.Monad.Trans.Writer.Lazy

-- utilities for building units
type AnonymousUnit = ([(Rational, BaseUnit)], [(Rational, BaseUnit)])
class DeriveUtil a where
    toFrac :: a -> AnonymousUnit

instance DeriveUtil BaseUnit where
    toFrac x = ([(1,x)],[])

instance DeriveUtil DerivedUnit where
    toFrac (DerivedUnit _ _ ts bs) = (ts,bs)

instance DeriveUtil AnonymousUnit where
    toFrac x = x

(>*) :: (DeriveUtil a, DeriveUtil b) => a -> b -> AnonymousUnit
(>*) a b = (aTop ++ bTop, aBot ++ bBot) where
    (aTop,aBot) = toFrac a
    (bTop,bBot) = toFrac b

(>/) :: (DeriveUtil a, DeriveUtil b) => a -> b -> AnonymousUnit
(>/) a b = (aTop ++ bBot, aBot ++ bTop) where
    (aTop,aBot) = toFrac a
    (bTop,bBot) = toFrac b

inv :: (DeriveUtil a) => a -> AnonymousUnit
inv = (\(a,b)->(b,a)) . toFrac

type UnitM = Writer [DerivedUnit]

unit :: String -> String -> AnonymousUnit -> UnitM DerivedUnit
unit abbrev name def = let r = uncurry (DerivedUnit abbrev name) def in
    tell [r] >> return r

-- generate SI prefixes for the given unit, excepting the unit itself
siPrefixes :: DerivedUnit -> UnitM ()
siPrefixes u = mapM_ (addPrefix u) prefixes where
    addPrefix :: DerivedUnit -> (String,String,Rational) -> UnitM DerivedUnit
    addPrefix (DerivedUnit a n ts bs) (p,pn,k) = unit (p++a) (pn++n)
        ((map (\(r,u)->(k*r, u)) ts),bs)
    prefixes :: [(String,String,Rational)]
    prefixes = [
        ("Y","yotta", 10^24),
        ("Z","zetta", 10^21),
        ("E","exa",   10^18),
        ("P","peta",  10^15),
        ("T","tera",  10^12),
        ("G","giga",  10^9),
        ("M","mega",  10^6),
        ("k","kilo",  10^3),
        ("c","centi", 10^(-2)),
        ("m","milli", 10^(-3)),
        ("μ","micro", 10^(-6)),
        ("n","nano",  10^(-9)),
        ("p","pico",  10^(-12)),
        ("f","femto", 10^(-15)),
        ("a","atto",  10^(-18)),
        ("z","zepto", 10^(-21)),
        ("y","yocto", 10^(-24))]

siPrefixed :: UnitM DerivedUnit -> UnitM DerivedUnit
siPrefixed m = m >>= (\x->siPrefixes x >> return x)

-- construct derived unit system
system :: [BaseUnit] -> UnitM a -> UnitSystem
system b w = let derived = snd $ runWriter w in UnitSystem {
    baseUnits = b, derivedUnits = derived }
