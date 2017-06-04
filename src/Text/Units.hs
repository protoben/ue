module Text.Units (unit) where

import Data.Units

import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Expr

import Control.Monad

type CParserT = ParsecT String ()

baseUnit :: Monad m => UnitSystem -> CParserT m BaseUnit
baseUnit = choice . map (try . parse') . baseUnits
    where
        parse' :: Monad m => BaseUnit -> CParserT m BaseUnit
        parse' u@(BaseUnit a _ _) = string a >> return u

derivedUnit :: Monad m => UnitSystem -> CParserT m DerivedUnit
derivedUnit = choice . map (try . parse') . derivedUnits
    where
        parse' :: Monad m => DerivedUnit -> CParserT m DerivedUnit
        parse' u@(DerivedUnit a _ _ _) = string a >> return u

atomicUnit :: Monad m => UnitSystem -> CParserT m DerivedUnit
atomicUnit = choice . map (try . parse') . atomicUnits
    where
        parse' :: Monad m => DerivedUnit -> CParserT m DerivedUnit
        parse' u@(DerivedUnit a _ _ _) = string a >> return u

systemUnit :: Monad m => UnitSystem -> CParserT m AnonymousUnit
systemUnit sys = choice [
        liftM toFrac $ try $ derivedUnit sys,
        liftM toFrac $ try $ baseUnit sys ]

systems = [mks]

-- parse a primitive unit, not part of any unit system
primUnit :: Monad m => CParserT m AnonymousUnit
primUnit = choice $ map (try . systemUnit) systems

-- parse an atomic unit
primAtomic :: Monad m => CParserT m AnonymousUnit
primAtomic = liftM toFrac $ choice $ map (try . atomicUnit) systems

type AU = AnonymousUnit

-- parse a composite unit
unit :: Monad m => CParserT m AnonymousUnit
unit = (try primAtomic) <|> (buildExpressionParser exprTable primUnit) where
    exprTable = [[lassoc '/' (>/), lassoc '*' (>*)]]
    lassoc :: Monad m => Char -> (AU -> AU -> AU) -> Operator String () m AU
    lassoc c o = Infix (char c >> return o) AssocLeft
