module Data.Expression (
    Expr(..), Value(..), BinOp(..), UnaryOp(..), Relation(..), VariableRef(..),
    containsSymbols, allSymbols,
    display, treeDepth, substM, subst, substVar, forceUnit) where

import Data.List
import Data.Maybe
import Data.Units
import Data.Display

import Control.Monad

data BinOp = Add | Subtract | Multiply | Divide | Power deriving (Show,Eq)
data UnaryOp = Negate deriving (Show, Eq)
data Relation = Equal | Lesser | Greater | LesserEqual
    | GreaterEqual deriving (Show, Eq)
type Name = String
type AUnit = AnonymousUnit

instance Displayable BinOp where
    display Add      = [(Symbol, "+")]
    display Subtract = [(Symbol, "-")]
    display Multiply = [(Symbol, "*")]
    display Divide   = [(Symbol, "/")]
    display Power    = [(Symbol, "^")]

instance Displayable UnaryOp where
    display Negate = [(Symbol, "-")]

instance Displayable Relation where
    display Equal        = [(Symbol, "=")]
    display Lesser       = [(Symbol, "<")]
    display Greater      = [(Symbol, ">")]
    display LesserEqual  = [(Symbol, "<=")]
    display GreaterEqual = [(Symbol, ">=")]

data Value = IntValue Integer AUnit | -- basic arbitrary-precision integer
    ExactReal Integer Integer AUnit | -- exact real D*10^P stored as (D,P) pair
    Vec2 Value Value | -- 2d vector, stored as (x,y)
    VecN [Value] -- N-dimensional vector
    deriving Show

data VariableRef = NamedRef Name deriving (Show,Eq,Ord)

instance Dimensioned Value where
    dimension (IntValue _ u) = dimension u
    dimension (ExactReal _ _ u) = dimension u
    dimension (Vec2 _ _) = Dimensionless
    dimension (VecN _) = Dimensionless

instance Displayable Value where
    display (IntValue i u) = (Numeric,show i):(display u)
    display (ExactReal n e u) = let s = show n in
        ((\(a,b)->(Numeric, concat [a, ".", b])) $
        splitAt (length s + fromIntegral e) s):(display u)
    display (Vec2 a b) = concat [(Numeric,"<"):(display a),
                                 (Numeric,","):(display b),
                                 [(Numeric, ">")]]
    display (VecN xs)  = concat [[(Numeric,"<")],
                                 intercalate [(Numeric,",")] $ map display xs,
                                 [(Numeric,">")]]

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
    (==) (ExactReal d p _) (ExactReal d' p' _) = (d == d') && (p == p')
    (==) _ _ = False

data Expr =
    RelationExpr Relation Expr Expr |
    BinaryExpr BinOp Expr Expr |
    UnaryExpr UnaryOp Expr |
    FuncCall Name [Expr] |
    NameRef VariableRef |
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
allSymbols (NameRef (NamedRef n)) = [n]
allSymbols (FuncCall _ _) = []
allSymbols (Constant _) = []
allSymbols (UnaryExpr _ e) = allSymbols e
allSymbols (BinaryExpr _ l r) = allSymbols l ++ allSymbols r
allSymbols (RelationExpr _ l r) = allSymbols l ++ allSymbols r

data ParentType = TopLevel | AddSub | Sub | Mul | Div | PowerLeft |
    PowerRight | Unary deriving Eq

pars :: [[(ContentClass,String)]] -> [(ContentClass, String)]
pars xs = (Symbol,"("):(concat $ xs ++ [[(Symbol,")")]])

-- Show an expression, properly parenthesized given the kind of expression it's
-- contained within.
display' :: ParentType -> Expr -> [(ContentClass,String)]
display' _ (RelationExpr o a b) =
    concat [display' TopLevel a, display o, display' TopLevel b]
display' p (BinaryExpr Add a b) =
    (if p `elem` [TopLevel, AddSub] then concat else pars)
    [display' AddSub a, display Add, display' AddSub b]
display' p (BinaryExpr Subtract a b) =
    (if p `elem` [TopLevel, AddSub] then concat else pars)
    [display' AddSub a, display Subtract, display' Sub b]
display' p (BinaryExpr Multiply a b) =
    (if p `elem` [TopLevel, AddSub, Sub, Mul] then concat else pars)
    [display' Mul a, display Multiply, display' Mul b]
display' p (BinaryExpr Divide a b) = concat
    [display' Div a, display Divide, display' Div b]
display' p (BinaryExpr Power a b) = concat
    [display' PowerLeft a, display Power, display' PowerRight b]
display' p (UnaryExpr Negate a) = (if p == TopLevel then concat else pars)
    [display Negate, display' Unary a]
display' _ (FuncCall nm args) = concat [[(Name,nm), (Symbol,"(")],
        intercalate [(Symbol,", ")] $ map (display' TopLevel) args,
        [(Symbol,")")]]
display' _ (NameRef (NamedRef x)) = [(Variable,x)]
display' _ (Constant c) = display c

instance Displayable Expr where
    display x = display' TopLevel x

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
    NameRef (NamedRef n) -> if n == tgt then (Just e) else Nothing
    _                    -> Nothing)
