module Data.Expression (
    Expr(..), Value(..), BinOp(..), UnaryOp(..), Relation(..),
    containsSymbols, allSymbols,
    display, treeDepth, substM, subst, substVar, forceUnit) where

import Data.List
import Data.Maybe
import Data.Units

import Control.Monad

data BinOp = Add | Subtract | Multiply | Divide | Power deriving (Show,Eq)
data UnaryOp = Negate deriving (Show, Eq)
data Relation = Equal | Lesser | Greater | LesserEqual
    | GreaterEqual deriving (Show, Eq)
type Name = String
type AUnit = AnonymousUnit

data Value = IntValue Integer AUnit | -- basic arbitrary-precision integer
    ExactReal Integer Integer AUnit | -- exact real D*10^P stored as (D,P) pair
    Vec2 Value Value | -- 2d vector, stored as (x,y)
    VecN [Value] -- N-dimensional vector
    deriving Show

instance Dimensioned Value where
    dimension (IntValue _ u) = dimension u
    dimension (ExactReal _ _ u) = dimension u
    dimension (Vec2 _ _) = Dimensionless
    dimension (VecN _) = Dimensionless

-- |Forcibly rewrite the units of a value
-- This function may lose information. Use with caution.
forceUnit :: (Unit u) => u -> Value -> Value
forceUnit u (IntValue i _) = IntValue i (toFrac u)
forceUnit u (ExactReal d p _) = ExactReal d p (toFrac u)
forceUnit u (Vec2 a b) = Vec2 (forceUnit u a) (forceUnit u b)
forceUnit u (VecN xs) = VecN $ map (forceUnit u) xs

-- TODO: Account for units that are multiples of each other
instance Eq Value where
    (==) (IntValue n _) (IntValue m _) = n == m
    (==) _ _ = False

data Expr =
    RelationExpr Relation Expr Expr |
    BinaryExpr BinOp Expr Expr |
    UnaryExpr UnaryOp Expr |
    FuncCall Name [Expr] |
    NameRef Name |
    Constant Value
    deriving (Show,Eq)

-- List whether an expression contains unbound symbols
containsSymbols :: Expr -> Bool
containsSymbols (NameRef _) = True
containsSymbols (FuncCall _ _) = True
containsSymbols (Constant _) = False
containsSymbols (UnaryExpr _ e) = containsSymbols e
containsSymbols (BinaryExpr _ a b) = containsSymbols a || containsSymbols b
containsSymbols (RelationExpr _ a b) = containsSymbols a || containsSymbols b

-- Get a list of all unbound symbols
allSymbols :: Expr -> [Name]
allSymbols (NameRef n) = [n]
allSymbols (FuncCall _ _) = []
allSymbols (Constant _) = []
allSymbols (UnaryExpr _ e) = allSymbols e
allSymbols (BinaryExpr _ l r) = allSymbols l ++ allSymbols r
allSymbols (RelationExpr _ l r) = allSymbols l ++ allSymbols r

displayVal :: Value -> String
displayVal (IntValue i u) = show i ++ displayUnit u
displayVal (ExactReal n e u) = let s = show n in
    (\(a,b)->concat [a,".",b,displayUnit u]) $
    splitAt (length s + fromIntegral e) s
displayVal (Vec2 a b) = concat ["<", displayVal a, ", ", displayVal b, ">"]
displayVal (VecN xs) = concat ["<", intercalate ", " $ map displayVal xs, ">"]

data ParentType = TopLevel | AddSub | Sub | Mul | Div | PowerLeft |
    PowerRight | Unary deriving Eq

pars :: [String] -> String
pars xs = '(':(concat $ xs ++ [")"])

-- Show an expression, properly parenthesized given the kind of expression it's
-- contained within.
display' :: ParentType -> Expr -> String
display' _ (RelationExpr Equal a b) = concat
    [display' TopLevel a, "=", display' TopLevel b]
display' _ (RelationExpr Lesser a b) = concat
    [display' TopLevel a, "<", display' TopLevel b]
display' _ (RelationExpr Greater a b) = concat
    [display' TopLevel a, ">", display' TopLevel b]
display' _ (RelationExpr LesserEqual a b) = concat
    [display' TopLevel a, "<=", display' TopLevel b]
display' _ (RelationExpr GreaterEqual a b) = concat
    [display' TopLevel a, ">=", display' TopLevel b]
display' p (BinaryExpr Add a b) =
    (if p `elem` [TopLevel, AddSub] then concat else pars)
    [display' AddSub a, "+", display' AddSub b]
display' p (BinaryExpr Subtract a b) =
    (if p `elem` [TopLevel, AddSub] then concat else pars)
    [display' AddSub a, "-", display' Sub b]
display' p (BinaryExpr Multiply a b) =
    (if p `elem` [TopLevel, AddSub, Sub, Mul] then concat else pars)
    [display' Mul a, "*", display' Mul b]
display' p (BinaryExpr Divide a b) = concat
    [display' Div a, "/", display' Div b]
display' p (BinaryExpr Power a b) = concat
    [display' PowerLeft a, "^", display' PowerRight b]
display' p (UnaryExpr Negate a) = (if p == TopLevel then concat else pars)
    ["-", display' Unary a]
display' _ (FuncCall nm args) = concat
    [nm, "(", intercalate ", " (map (display' TopLevel) args), ")"]
display' _ (NameRef x) = x
display' _ (Constant c) = displayVal c

display :: Expr -> String
display = display' TopLevel

treeDepth :: (Num a, Ord a) => Expr -> a
treeDepth (RelationExpr _ l r) = 1 + (max (treeDepth l) (treeDepth r))
treeDepth (BinaryExpr _ l r) = 1 + (max (treeDepth l) (treeDepth r))
treeDepth (UnaryExpr o x) = (if o == Negate then 0 else 1) + (treeDepth x)
treeDepth (FuncCall _ xs) = 1 + (foldl' max 0 $ map treeDepth xs)
treeDepth (NameRef _) = 1
treeDepth (Constant _) = 1

fromMaybeM :: (Monad m) => m (Maybe b) -> m b -> m b
fromMaybeM a b = a >>= (\x->case x of
    Nothing -> b
    Just r  -> return r)

-- Perform a substitution over each element in the tree. The results from the
-- substitution function are not themselves substituted, to prevent infinite
-- recursion. When the substitution function returns Nothing, the passed element
-- will not be modified.
substM :: Monad m => (Expr -> m (Maybe Expr)) -> Expr -> m Expr
substM f x@(RelationExpr rel l r) = fromMaybeM (f x) $
    liftM2 (RelationExpr rel) (substM f l) (substM f r)
substM f x@(BinaryExpr op l r) = fromMaybeM (f x) $
    liftM2 (BinaryExpr op) (substM f l) (substM f r)
substM f x@(UnaryExpr op e) = fromMaybeM (f x) $
    liftM (UnaryExpr op) (substM f e)
substM f x@(FuncCall n es) = fromMaybeM (f x) $ liftM (FuncCall n) $
    mapM (substM f) es
substM f x@(NameRef n) = fromMaybeM (f x) $ return x
substM f x@(Constant v) = fromMaybeM (f x) $ return x

subst :: (Expr -> Maybe Expr) -> Expr -> Expr
subst f = head . substM (return . f)

-- Utility function for substituting a single variable
substVar :: String -> Expr -> Expr -> Expr
substVar tgt e = subst (\x->case x of
    NameRef n -> if n == tgt then (Just e) else Nothing
    _         -> Nothing)
