module Text.Units where

import Data.Units

import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Expr

import Control.Monad

parseBaseUnit :: UnitSystem -> Parser BaseUnit
parseBaseUnit = choice . map (try . parse') . baseUnits
    where
        parse' :: BaseUnit -> Parser BaseUnit
        parse' u@(BaseUnit a _ _) = string a >> return u

parseDerivedUnit :: UnitSystem -> Parser DerivedUnit
parseDerivedUnit = choice . map (try . parse') . derivedUnits
    where
        parse' :: DerivedUnit -> Parser DerivedUnit
        parse' u@(DerivedUnit a _ _ _) = string a >> return u

parseSystemUnit :: UnitSystem -> Parser AnonymousUnit
parseSystemUnit sys = choice [
        liftM toFrac $ try $ parseBaseUnit sys,
        liftM toFrac $ try $ parseDerivedUnit sys ]

systems = [mks]

-- parse a primitive unit, not part of any unit system
parsePrimUnit :: Parser AnonymousUnit
parsePrimUnit = choice $ map (try . parseSystemUnit) systems

-- parse a composite unit
parseUnit :: Parser AnonymousUnit
parseUnit = choice [
        liftM2 (>/) parsePrimUnit (char '/' >> parsePrimUnit)
    ]
