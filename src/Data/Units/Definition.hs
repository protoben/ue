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

-- construct derived unit system
system :: [BaseUnit] -> UnitM a -> UnitSystem
system b w = let derived = snd $ runWriter w in UnitSystem {
    baseUnits = b, derivedUnits = derived }
