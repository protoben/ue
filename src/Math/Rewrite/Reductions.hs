module Math.Rewrite.Reductions (
    reduce, reductions,
    simplifyArithmetic, onChange, reorderLikeTerms) where
-- this module contains various algorithmic reductions which help simplify
-- expressions
import Data.Expression
import Data.Units

import Data.List
import Data.Maybe
import Data.Function

import Math.Approximate
import Math.Rewrite.Engine

import Control.Applicative

-- Utility function - take a function and produce a result only if it changes
-- the input.
onChange :: Eq a => (a -> a) -> a -> Maybe a
onChange f x = let v = f x in if x == v then Nothing else Just v

-- Extract lists of terms connected by addition and subtraction, converting the
-- subtracted terms to their negated forms.
collapseSum :: Expr -> [Expr]
collapseSum (BinaryExpr Add a b) = (collapseSum a) ++ (collapseSum b)
collapseSum (BinaryExpr Subtract a b) = (UnaryExpr Negate b) : (collapseSum a)
collapseSum e = [e]

-- Extract lists of terms connected by multiplication
collapseProd :: Expr -> [Expr]
collapseProd (BinaryExpr Multiply a b) = (collapseProd a) ++ (collapseProd b)
collapseProd e = [e]

-- Expand a list of summation terms into a tree of add/subtract operations
expandSum :: [Expr] -> Expr
expandSum = foldl1' join
    where
        join l (UnaryExpr Negate r) = BinaryExpr Subtract l r
        join l r = BinaryExpr Add l r

-- Expand a list of product terms into a tree of multiply operations
expandProd :: [Expr] -> Expr
expandProd = foldl1' (BinaryExpr Multiply)

-- Reorder a list of elements so equal terms are adjacent, and return the
-- groups. If all equal terms are already adjacent, don't change the input.
equalGroup :: Eq a => [a] -> [[a]]
equalGroup xs = fst $ fromJust $ find (null . snd) $
    iterate gatherMore ([], xs) where
    gatherMore (gs,(x:xs)) = let (g,ys) = partition (== x) xs in (gs ++ [x:g], ys)

-- just a local instance to make dimensions sortable for grouping
-- doesn't really implement anything specifically
instance Ord Dimension where
    compare Dimensionless Dimensionless = EQ
    compare (Dimension xs ys) (Dimension xs' ys') = foldl' mappend EQ
        [compare xs xs',
         compare ys ys']
    compare Dimensionless (Dimension _ _) = LT
    compare (Dimension _ _) Dimensionless = GT

-- Reorder a list of expressions to group similar ones
sortExprGroups :: [[Expr]] -> [[Expr]]
sortExprGroups = sortBy (\(a:_) (b:_)->mconcat $ map (\f->f a b)
            [depth, naming, dimensions, constSize]) where
        depth :: Expr -> Expr -> Ordering
        depth = compare `on` treeDepth

        naming :: Expr -> Expr -> Ordering
        naming (NameRef a) (NameRef b) = compare a b
        naming a@(NameRef _) (BinaryExpr _ l r) =
            (naming a l) `mappend` (naming a r)
        naming a@(NameRef _) (Constant _) = GT
        naming (UnaryExpr _ a) b = naming a b
        naming a (UnaryExpr _ b) = naming a b
        naming (BinaryExpr _ l r) b@(NameRef _) =
            (naming l b) `mappend` (naming r b)
        naming (Constant _) b@(NameRef _) = LT
        naming _ _ = EQ

        constSize :: Expr -> Expr -> Ordering
        constSize (Constant a) (Constant b) = compareValues a b
        constSize (UnaryExpr _ a) b = constSize a b
        constSize a (UnaryExpr _ b) = constSize a b
        constSize _ _ = EQ

        dimensions :: Expr -> Expr -> Ordering
        dimensions (Constant u) (Constant v) = compare (dimension u) (dimension v)
        dimensions (UnaryExpr _ a) v = dimensions a v
        dimensions u (UnaryExpr _ a) = dimensions u a
        dimensions (BinaryExpr _ a b) v =
            (dimensions a v) `mappend` (dimensions b v)
        dimensions u (BinaryExpr _ a b) =
            (dimensions u a) `mappend` (dimensions u b)
        dimensions _ _ = EQ

-- REDUCTION: Reorder commutative operations so like terms are adjacent
reorderLikeTerms :: Expr -> Expr
reorderLikeTerms e@(BinaryExpr Subtract a b) = expandSum $ map expandSum $ sortExprGroups $ equalGroup $ collapseSum e
reorderLikeTerms e@(BinaryExpr Add a b) = expandSum $ map expandSum $ sortExprGroups $ equalGroup $ collapseSum e
reorderLikeTerms e@(BinaryExpr Multiply a b) = expandProd $ map expandProd $ sortExprGroups $ equalGroup $ collapseProd e
reorderLikeTerms e = e

maybeEither :: Either a b -> Maybe b
maybeEither (Left _) = Nothing
maybeEither (Right r) = Just r

-- REDUCTION: Simplify arithmetic expressions by evaluating them
simplifyArithmetic :: Reduction
simplifyArithmetic e@(BinaryExpr Add (Constant _) (Constant _)) =
    maybeEither $ approx e
simplifyArithmetic e@(BinaryExpr Subtract (Constant _) (Constant _)) =
    maybeEither $ approx e
simplifyArithmetic e@(BinaryExpr Multiply (Constant _) (Constant _)) =
    maybeEither $ approx e
simplifyArithmetic e@(BinaryExpr Power -- whether to simplify constant chosen by heuristic
    (Constant (IntValue a u)) (Constant (IntValue b v))) =
        if a^b < 1000 then maybeEither $ approx e else Nothing
simplifyArithmetic e@(BinaryExpr Divide (Constant (IntValue a u))
                                        (Constant (IntValue b v))) =
        if (a `mod` b) == 0 then maybeEither $ approx e else Nothing
simplifyArithmetic e@(UnaryExpr Negate (Constant _)) = maybeEither $ approx e
simplifyArithmetic _ = Nothing

-- Utility function for expression binding. Applies the function to the first
-- list item for which it returns Just, then substitutes that into the rest of
-- the list.
applyOnce :: (a -> Maybe a) -> [a] -> Maybe [a]
applyOnce f [] = Nothing
applyOnce f (x:xs) = (fmap (:xs) $ f x) <|> (fmap (x:) $ applyOnce f xs)

-- Perform reduction on a subexpression, if possible. Return the original expr
-- with the altered subexpr substituted in.
reduceSubexpr :: [Reduction] -> Reduction
reduceSubexpr rs (BinaryExpr o l r) =
    (fmap (\x->BinaryExpr o x r) $ reduce rs l) <|>
    (fmap (\x->BinaryExpr o l x) $ reduce rs r)
reduceSubexpr rs (UnaryExpr o e) = (UnaryExpr o) <$> reduce rs e
reduceSubexpr rs (FuncCall n args) = (FuncCall n) <$> applyOnce (reduce rs) args
reduceSubexpr rs (RelationExpr o l r) =
    (fmap (\x->RelationExpr o x r) $ reduce rs l) <|>
    (fmap (\x->RelationExpr o l x) $ reduce rs r)
reduceSubexpr rs _ = Nothing

-- Perform one reduction on an expression
reduce :: [Reduction] -> Expr -> Maybe Expr
reduce rs e = (foldl1' (<|>) $ map (\f->f e) rs) <|> (reduceSubexpr rs e)

-- Build a potentially-infinite chain of successive reductions for an expression
reductions :: [Reduction] -> Expr -> [Expr]
reductions rs = unfoldr (fmap (\e->(e,e)) . (reduce rs))
