module Math.Rewrite.Engine (
    IDType(..), RRExpr(..), RewriteRule(..), Reduction(..),
    -- running rules
    rewrite,

    -- rule construction utilities
    (=+), (=-), (=*), (=/), (=^), neg,
    (=:),
    csti,
    ) where
-- This module implements the expression rewriting engine and utilities for
-- defining rewrite rules.

import Data.Expression
import Data.Units
import Data.List
import Data.Maybe

import Control.Applicative

data IDType = Any | Literal | Nonliteral deriving Show

data RRExpr =
    RRRelated Relation RRExpr RRExpr |
    RRBinary BinOp RRExpr RRExpr |
    RRUnary UnaryOp RRExpr |
    RRIdentifier IDType Char |
    RRExpr Expr
    deriving Show

type RewriteRule = (RRExpr, RRExpr)
type Reduction = Expr -> Maybe Expr

-- Check whether there are any conflicts in a match result and return Nothing if
-- any are found.
checkConflicts :: Maybe [(Char, Expr)] -> Maybe [(Char, Expr)]
checkConflicts Nothing = Nothing
checkConflicts (Just e) = let env = nub e in
    if length env /= (length $ nubBy (\l r->fst l == fst r) env) then Nothing
    else Just e

-- Try to match a rewrite rule to the given expression, and return the bindings
-- for each symbol
matchRule :: Expr -> RRExpr -> Maybe [(Char, Expr)]
matchRule e@(Constant _) (RRIdentifier Literal c) = Just [(c, e)]
matchRule e (RRIdentifier Nonliteral c) = if containsSymbols e then Just [(c,e)] else Nothing
matchRule e (RRIdentifier Any c) = Just [(c, e)]
matchRule (RelationExpr o l r) (RRRelated o' a b) = if o /= o' then Nothing else
    checkConflicts $ fmap concat $ sequence [matchRule l a, matchRule r b]
matchRule (BinaryExpr o l r) (RRBinary o' a b) = if o /= o' then Nothing else
    checkConflicts $ fmap concat $ sequence [matchRule l a, matchRule r b]
matchRule (UnaryExpr o e) (RRUnary o' r) = if o /= o' then Nothing else
    matchRule e r
matchRule e (RRExpr e') = if e == e' then Just [] else Nothing
matchRule _ _ = Nothing

-- Substitute expressions in for identifiers in a rewrite expr, and return the
-- resulting expression
bindRule :: [(Char, Expr)] -> RRExpr -> Maybe Expr
bindRule e (RRRelated o a b) = RelationExpr o <$> (bindRule e a) <*> (bindRule e b)
bindRule e (RRBinary o a b) = BinaryExpr o <$> (bindRule e a) <*> (bindRule e b)
bindRule e (RRUnary o a) = UnaryExpr o <$> bindRule e a
bindRule e (RRIdentifier _ c) = lookup c e
bindRule e (RRExpr e') = Just e'

-- Apply one rule of a setand return the reduced expression, if any rule applies
rewrite :: [RewriteRule] -> Reduction
rewrite rules e = (case matches of
        []    -> Nothing
        (x:_) -> uncurry bindRule x) where
    matches = mapMaybe (\(s,t)->fmap (\x->(x,t)) $ matchRule e s) rules

-- utilities for rule construction begin here
infixl 7 =+
infixl 7 =-
infixl 8 =*
infixl 8 =/
infixl 9 =^

-- unary and binary expressions
(=+) :: RRExpr -> RRExpr -> RRExpr
a =+ b = RRBinary Add a b
(=-) :: RRExpr -> RRExpr -> RRExpr
a =- b = RRBinary Subtract a b
(=*) :: RRExpr -> RRExpr -> RRExpr
a =* b = RRBinary Multiply a b
(=/) :: RRExpr -> RRExpr -> RRExpr
a =/ b = RRBinary Divide a b
(=^) :: RRExpr -> RRExpr -> RRExpr
a =^ b = RRBinary Power a b
neg :: RRExpr -> RRExpr
neg a = RRUnary Negate a

infix 6 =:

-- relational expressions
(=:) :: RRExpr -> RRExpr -> RRExpr
a =: b = RRRelated Equal a b

-- other utilities
cst :: Value -> RRExpr
cst = RRExpr . Constant

csti :: Integer -> RRExpr
csti n = cst (IntValue n noUnit)
