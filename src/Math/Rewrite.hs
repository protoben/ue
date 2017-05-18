module Math.Rewrite (reductions, simplify) where

import Data.List
import Data.Maybe
import Data.Function
import Data.Expression

import Control.Monad
import Control.Applicative

data IDType = Any | Literal | Nonliteral deriving Show

data RRExpr =
    RRRelated Relation RRExpr RRExpr |
    RRBinary BinOp RRExpr RRExpr |
    RRUnary UnaryOp RRExpr |
    RRIdentifier IDType Char |
    RRExpr Expr
    deriving Show

infix 6 =:
infixl 7 =+
infixl 7 =-
infixl 8 =*
infixl 8 =/
infixl 9 =^

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

(=:) :: RRExpr -> RRExpr -> RRExpr
a =: b = RRRelated Equal a b

type RewriteRule = (RRExpr, RRExpr)
type Reduction = Expr -> Maybe Expr

rewriteRules :: [RewriteRule]
rewriteRules =
    let (a:b:c:d:e:f:[]) = map (RRIdentifier Any) "abcdef"
        (x:y:z:[]) = map (RRIdentifier Nonliteral) "xyz"
        (m:n:k:[]) = map (RRIdentifier Literal) "mnk" in [
    -- reordering terms to normal form
    (a =* n, n =* a),

    -- simple properties
    (a =- a, csti 0), (a =- csti 0, a), (csti 0 =- a, neg a),
    (neg a =+ a, csti 0), (a =+ (neg a), csti 0),
    (a =+ csti 0, a), (csti 0 =+ a, a),
    (a =* csti 0, csti 0), (csti 0 =* a, csti 0),
    (a =* csti 1, a), (csti 1 =* a, a),
    (a =/ a, csti 1),

    -- unary operations
    (neg $ neg a, a),

    -- addition/subtraction
    (a =+ neg a, csti 0), (a =- neg a, a =+ a),
    (a =+ (b =+ neg a), b), (a =+ (neg a =+ b), b),
    (a =+ (b =+ a), b =+ a =* csti 2), (a =+ (a =+ b), b =+ a =* csti 2),
    ((b =+ a) =+ a, b =+ a =* csti 2), ((a =+ b) =+ a, b =+ a =* csti 2),

    -- multiplication/division
    (a =* (b =/ a), b), ((b =/ a) =* a, b), ((a =* b) =/ a, b), ((b =* a) =/ a, b),
    ((n =* a) =/ m, (n =/ m) =* a), ((a =* n) =/ m, (n =/ m) =* a),
    (a =+ a, csti 2 =* a),
    (a =+ b=*a, (b =+ csti 1) =* a), (a =+ a=*b, (b =+ csti 1) =* a),
    (a =- b=*a, (b =- csti 1) =* a), (a =- a=*b, (b =- csti 1) =* a),
    ((neg a) =+ b=*a, (b =- csti 1) =* a), ((neg a) =+ a=*b, (b =- csti 1) =* a),
    (a=*b =+ a=*c, a =* (b =+ c)), (b=*a =+ c=*a, a =* (b =+ c)), -- factoring out coefficients
    (a =- (b =+ c), a =+ (neg b =+ neg c)), -- distribute subtraction
    (a =- (b =- c), a =+ (neg b =+ c)), -- distribute subtraction
    ((a =+ b=*c) =+ d=*c, (a =+ c=*(b =+ d))),
    ((a =+ b=*c) =- d=*c, (a =+ c=*(b =- d))),

    -- division
    (a=/b =+ c=/b, (a =+ c) =/ b),

    -- exponentiation
    (a =^ csti 0, csti 1), (a =^ csti 1, a), (a =^ csti (-1), (csti 1) =/ a),
    (csti 1 =^ a, a), (csti 0 =^ a, csti 0),
    (a =* a, a =^ csti 2),
    (a =* a=^b, a =^ (b =+ csti 1)), (a=^b =* a, a =^ (b =+ csti 1)),
    (a=^b =/ a, a =^ (b =- csti 1)), (a=^b =/ a=^c, a =^ (b =- c)),
    (a=^b =* a=^c, a =^ (b =+ c)),
    ((d =* a=^b) =* a=^c, d =* (a =^ (b =+ c))),
    ((a=^b =* d) =* a=^c, d =* (a =^ (b =+ c))),
    ((a =^ b) =* (d =* (a =^ c)), d =* (a =^ (b =+ c))),
    ((a =^ b) =* ((a =^ c) =* d), d =* (a =^ (b =+ c))),

    ((a =^ b) =^ c, a =^ (b =* c)),
    ((a =* b) =^ m, (a =^ m) =* (b =^ m)),
    ((m =^ a) =* (n =^ a), (m =* n) =^ a),

    -- basic equality simplifications
    -- only do this for nonliterals for now, until we can produce boolean values
    (x =: x, csti 1 =: csti 1),
    (n =: x, x =: n), -- constants always on right side of equality
    (a =* b =: a =* c, b =: c), (b =* a =: a =* c, b =: c),
    (a =* b =: c =* a, b =: c), (b =* a =: c =* a, b =: c),
    (a =* b =: a, b =: csti 1), (b =* a =: a, b =: csti 1),
    (a =+ b =: c =+ b, a =: c), (b =+ a =: c =+ b, a =: c), -- subtract both sides
    (a =+ b =: b =+ c, a =: c), (b =+ a =: b =+ c, a =: c), -- subtract both sides
    (a =+ b =: m =* b, a =: b =* (m =- csti 1)),  -- subtract from multiplication
    (a =+ b =: m =* a, b =: a =* (m =- csti 1))
    ]
    where
        cst :: Value -> RRExpr
        cst = RRExpr . Constant

        csti = cst . IntValue

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

-- Utility function for expression binding. Applies the function to the first
-- list item for which is returns Just, then substitutes that into the rest of
-- the list.
applyOnce :: (a -> Maybe a) -> [a] -> Maybe [a]
applyOnce f [] = Nothing
applyOnce f (x:xs) = (fmap (:xs) $ f x) <|> (fmap (x:) $ applyOnce f xs)

-- Utility function - take a function and produce a result only if it changes
-- the input.
onChange :: Eq a => (a -> a) -> a -> Maybe a
onChange f x = let v = f x in if x == v then Nothing else Just v

-- Apply one rule and return the reduced expression, if any rule applies
rewrite :: Reduction
rewrite e = if null matches then Nothing else rewritten
    where
        rewritten :: Maybe Expr
        rewritten = uncurry bindRule $ head matches
        matches = mapMaybe (\(s,t)->fmap (\x->(x,t)) $ matchRule e s) rewriteRules

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

-- Reorder a list of expressions by (tree depth, variable name)
sortExprGroups :: [[Expr]] -> [[Expr]]
sortExprGroups = sortBy (\(a:_) (b:_)->mconcat $ map (\f->f a b) [depth, naming])
    where
        depth :: Expr -> Expr -> Ordering
        depth = compare `on` treeDepth

        naming :: Expr -> Expr -> Ordering
        naming (NameRef a) (NameRef b) = compare a b
        naming a@(NameRef _) (BinaryExpr _ l r) = (naming a l) `mappend` (naming a r)
        naming a@(NameRef _) (Constant _) = GT
        naming (UnaryExpr _ a) b = naming a b
        naming a (UnaryExpr _ b) = naming a b
        naming (BinaryExpr _ l r) b@(NameRef _) = (naming l b) `mappend` (naming r b)
        naming (Constant _) b@(NameRef _) = LT
        naming _ _ = EQ

-- Reorder chains of commutative operations so like terms are adjacent.
reorderLikeTerms :: Expr -> Expr
reorderLikeTerms e@(BinaryExpr Subtract a b) = expandSum $ map expandSum $ sortExprGroups $ equalGroup $ collapseSum e
reorderLikeTerms e@(BinaryExpr Add a b) = expandSum $ map expandSum $ sortExprGroups $ equalGroup $ collapseSum e
reorderLikeTerms e@(BinaryExpr Multiply a b) = expandProd $ map expandProd $ sortExprGroups $ equalGroup $ collapseProd e
reorderLikeTerms e = e

-- Simplify integer arithmetic expressions
simplifyArithmetic :: Reduction
simplifyArithmetic (BinaryExpr Add
    (Constant (IntValue a))
    (Constant (IntValue b))) = Just $ Constant $ IntValue $ a+b
simplifyArithmetic (BinaryExpr Subtract
    (Constant (IntValue a))
    (Constant (IntValue b))) = Just $ Constant $ IntValue $ a-b
simplifyArithmetic (BinaryExpr Multiply
    (Constant (IntValue a))
    (Constant (IntValue b))) = Just $ Constant $ IntValue $ a*b
simplifyArithmetic (BinaryExpr Power -- whether to simplify constant chosen by heuristic
    (Constant (IntValue a))
    (Constant (IntValue b))) = if (a^b < 1000) then
        Just $ Constant $ IntValue $ a^b else Nothing
simplifyArithmetic (BinaryExpr Divide
    (Constant (IntValue a))
    (Constant (IntValue b))) = if (a `mod` b) == 0 then
        (Just $ Constant $ IntValue $ (a `div` b)) else Nothing
simplifyArithmetic (UnaryExpr Negate (Constant (IntValue n))) =
    Just $ Constant $ IntValue $ negate n
simplifyArithmetic _ = Nothing

-- Perform reduction on a subexpression, if possible. Return the original expr
-- with the altered subexpr substituted in.
reduceSubexpr :: Reduction
reduceSubexpr (BinaryExpr o l r) =
    (fmap (\x->BinaryExpr o x r) $ reduceExpr l) <|>
    (fmap (\x->BinaryExpr o l x) $ reduceExpr r)
reduceSubexpr (UnaryExpr o e) = (UnaryExpr o) <$> reduceExpr e
reduceSubexpr (FuncCall n args) = (FuncCall n) <$> applyOnce reduceExpr args
reduceSubexpr (RelationExpr o l r) =
    (fmap (\x->RelationExpr o x r) $ reduceExpr l) <|>
    (fmap (\x->RelationExpr o l x) $ reduceExpr r)
reduceSubexpr _ = Nothing

-- Perform one reduction on an expression
reduceExpr :: Expr -> Maybe Expr
reduceExpr e = foldl1' (<|>) $ map (\f->f e) [
    simplifyArithmetic,        -- eagerly simplify integer arithmetic
    onChange reorderLikeTerms, -- then group terms
    rewrite,                   -- try to perform complex rewrite operations
    reduceSubexpr]             -- or try to reduce a subexpression

-- Build a potentially-infinite chain of successive reductions for an expression
reductions :: Expr -> [Expr]
reductions = unfoldr (\x->fmap (\e->(e,e)) $ reduceExpr x)

simplify :: Expr -> Expr
simplify e = if null (reductions e) then e else (last $ reductions e)
