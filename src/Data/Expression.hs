module Data.Expression (Expr(..), Value(..), BinOp(..), UnaryOp(..), Relation(..),
    containsSymbols, display, treeDepth) where

import Data.List

data BinOp = Add | Subtract | Multiply | Divide | Power deriving (Show,Eq)
data UnaryOp = Negate deriving (Show, Eq)
data Relation = Equal | Lesser | Greater | LesserEqual | GreaterEqual deriving (Show, Eq)
type Name = String

data Value = IntValue Integer | -- basic arbitrary-precision integer
    ExactReal Integer Integer | -- exact real D*10^P stored as (D,P) pair
    Vec2 Value Value | -- 2d vector, stored as (x,y)
    VecN [Value] -- N-dimensional vector
    deriving (Show,Eq)

data Expr =
    RelationExpr Relation Expr Expr |
    BinaryExpr BinOp Expr Expr |
    UnaryExpr UnaryOp Expr |
    FuncCall Name [Expr] |
    NameRef Name |
    Constant Value
    deriving (Show,Eq)

containsSymbols :: Expr -> Bool
containsSymbols (NameRef _) = True
containsSymbols (FuncCall _ _) = True
containsSymbols (Constant _) = False
containsSymbols (UnaryExpr _ e) = containsSymbols e
containsSymbols (BinaryExpr _ a b) = containsSymbols a || containsSymbols b

displayVal :: Value -> String
displayVal (IntValue i) = show i
displayVal (ExactReal n e) = (\(a,b)->concat [a,".",b]) $
    splitAt (fromIntegral e) $ show n
displayVal (Vec2 a b) = concat ["<", displayVal a, ", ", displayVal b, ">"]
displayVal (VecN xs) = concat ["<", intercalate ", " $ map displayVal xs, ">"]

data ParentType = TopLevel | AddSub | Sub | Mul | Div | PowerLeft | PowerRight | Unary deriving Eq

pars :: [String] -> String
pars xs = '(':(concat $ xs ++ [")"])

-- Show an expression, properly parenthesized given the kind of expression it's
-- contained within.
display' :: ParentType -> Expr -> String
display' _ (RelationExpr Equal a b) = concat [display' TopLevel a, "=", display' TopLevel b]
display' _ (RelationExpr Lesser a b) = concat [display' TopLevel a, "<", display' TopLevel b]
display' _ (RelationExpr Greater a b) = concat [display' TopLevel a, ">", display' TopLevel b]
display' _ (RelationExpr LesserEqual a b) = concat [display' TopLevel a, "<=", display' TopLevel b]
display' _ (RelationExpr GreaterEqual a b) = concat [display' TopLevel a, ">=", display' TopLevel b]
display' p (BinaryExpr Add a b) = (if p `elem` [TopLevel, AddSub] then concat else pars) [display' AddSub a, "+", display' AddSub b]
display' p (BinaryExpr Subtract a b) = (if p `elem` [TopLevel, AddSub] then concat else pars) [display' AddSub a, "-", display' Sub b]
display' p (BinaryExpr Multiply a b) = (if p `elem` [TopLevel, AddSub, Sub, Mul] then concat else pars) [display' Mul a, "*", display' Mul b]
display' p (BinaryExpr Divide a b) = concat [display' Div a, "/", display' Div b]
display' p (BinaryExpr Power a b) = concat [display' PowerLeft a, "^", display' PowerRight b]
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
treeDepth (UnaryExpr _ x) = 1 + (treeDepth x)
treeDepth (FuncCall _ xs) = 1 + (foldl' max 0 $ map treeDepth xs)
treeDepth (NameRef _) = 1
treeDepth (Constant _) = 1
