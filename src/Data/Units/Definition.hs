module Data.Units.Definition where

import Data.Ratio
import Data.Units.Types
import Control.Monad.Trans.Writer.Lazy

-- utilities for building units
type IsAtomic = Bool
type UnitM = Writer [(IsAtomic,DerivedUnit)]

unit :: String -> String -> AnonymousUnit -> UnitM DerivedUnit
unit abbrev name def = tell [(False,r)] >> return r
    where
        r = uncurry (DerivedUnit abbrev name) tup
        (AnonymousUnit tup) = def

-- make all definitions from the subexpression atomic
atomic :: UnitM a -> UnitM a
atomic = censor (map (\(_,b)->(True,b)))

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
    ("c","centi", 1 % 10^2),
    ("m","milli", 1 % 10^3),
    ("u","micro", 1 % 10^6),
    ("n","nano",  1 % 10^9),
    ("p","pico",  1 % 10^12),
    ("f","femto", 1 % 10^15),
    ("a","atto",  1 % 10^18),
    ("z","zepto", 1 % 10^21),
    ("y","yocto", 1 % 10^24)]

-- generate SI prefixes for the given unit, excepting the unit itself
siPrefixes :: DerivedUnit -> UnitM ()
siPrefixes u = mapM_ (addPrefix u) prefixes where
    addPrefix :: DerivedUnit -> (String,String,Rational) -> UnitM DerivedUnit
    addPrefix (DerivedUnit a n ts bs) (p,pn,k) = unit (p++a) (pn++n) $
        AnonymousUnit ((map (\(r,u)->(k*r, u)) ts),bs)

-- generate SI prefixes for the given base unit, excepting the unit itself
siPrefixesBase :: BaseUnit -> UnitM ()
siPrefixesBase u = mapM_ (addPrefix u) prefixes where
    addPrefix :: BaseUnit -> (String,String,Rational) -> UnitM DerivedUnit
    addPrefix u@(BaseUnit a n _) (p,pn,k) = unit (p++a) (pn++n) $
        AnonymousUnit ([(k,u)], [])

siPrefixed :: UnitM DerivedUnit -> UnitM DerivedUnit
siPrefixed m = m >>= (\x->siPrefixes x >> return x)

siPrefixedBase :: BaseUnit -> UnitM ()
siPrefixedBase u = siPrefixesBase u

-- construct derived unit system
system :: [BaseUnit] -> UnitM a -> UnitSystem
system b w = UnitSystem {
        baseUnits = b,
        atomicUnits = atomic,
        derivedUnits = derived } where
    res = snd $ runWriter w
    derived = map snd $ filter (not . fst) $ res
    atomic = map snd $ filter fst $ res
