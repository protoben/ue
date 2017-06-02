module Text.Units (unit) where

import Data.Units

import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Expr

import Control.Monad

baseUnit :: UnitSystem -> Parser BaseUnit
baseUnit = choice . map (try . parse') . baseUnits
    where
        parse' :: BaseUnit -> Parser BaseUnit
        parse' u@(BaseUnit a _ _) = string a >> return u

derivedUnit :: UnitSystem -> Parser DerivedUnit
derivedUnit = choice . map (try . parse') . derivedUnits
    where
        parse' :: DerivedUnit -> Parser DerivedUnit
        parse' u@(DerivedUnit a _ _ _) = string a >> return u

atomicUnit :: UnitSystem -> Parser DerivedUnit
atomicUnit = choice . map (try . parse') . atomicUnits
    where
        parse' :: DerivedUnit -> Parser DerivedUnit
        parse' u@(DerivedUnit a _ _ _) = string a >> return u

systemUnit :: UnitSystem -> Parser AnonymousUnit
systemUnit sys = choice [
        liftM toFrac $ try $ derivedUnit sys,
        liftM toFrac $ try $ baseUnit sys ]

systems = [mks]

-- parse a primitive unit, not part of any unit system
primUnit :: Parser AnonymousUnit
primUnit = choice $ map (try . systemUnit) systems

-- parse an atomic unit
primAtomic :: Parser AnonymousUnit
primAtomic = liftM toFrac $ choice $ map (try . atomicUnit) systems

type AU = AnonymousUnit

-- parse a composite unit
unit :: Parser AnonymousUnit
unit = (try primAtomic) <|> (buildExpressionParser exprTable primUnit) where
    exprTable = [[lassoc '/' (>/), lassoc '*' (>*)]]
    lassoc :: Monad m => Char -> (AU -> AU -> AU) -> Operator String () m AU
    lassoc c o = Infix (char c >> return o) AssocLeft
