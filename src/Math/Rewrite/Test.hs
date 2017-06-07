module Math.Rewrite.Test (tests) where

import Data.Expression
import Data.Units
import Math.Rewrite

import Data.List
import Control.Monad

import Test.QuickCheck as Q
import Distribution.TestSuite

satisfy :: Testable p => String -> p -> Test
satisfy n p = Test $ TestInstance {
        run = quickCheckResult p >>= (\x->return $ case x of
            (Success _ _ _) -> Finished Pass
            (GaveUp _ _ _)  -> Finished Pass
            (Failure _ _ _ _ _ _ r _ _ _)  -> Finished $ Fail r
            (NoExpectedFailure _ _ _) -> Finished $ Fail "Expected failing property"
            (InsufficientCoverage _ _ _) -> Finished $ Fail "Insufficient coverage"
        ),
        name = n,
        tags = [],
        options = [],
        setOption = (\_ _ -> Left "")
    }


rewriteTest :: String -> Gen Expr -> Expr -> Test
rewriteTest nm src dst = satisfy nm (forAll src (\e->simplify e == dst))

-- test that all the given expressions reduce to a target
reduces :: String -> [Expr] -> Expr -> Test
reduces n xs e = satisfy n $ all (\x->simplify x == e) xs

-- test that the given expression reduces to a given target
reduces' :: String -> Expr -> Expr -> Test
reduces' n x e = satisfy n $ simplify x == e

-- generate more complex alternate forms for the given expr
--genAlternate :: Expr -> Gen Expr

-- produce a constant integer
csti :: Integer -> Expr
csti = Constant . (\x->IntValue x noUnit)

-- produce a constant real number
cstf :: Integer -> Integer -> Expr
cstf n p = Constant $ ExactReal n p noUnit

-- generate a summation of the passed exprs
sumExprs :: [Expr] -> Expr
sumExprs [x] = x
sumExprs (x:xs) = BinaryExpr Add x $ sumExprs xs

-- Generate a tree of division/multiplication operations which results in 1
divMulTreeGen :: Gen Expr
divMulTreeGen = oneof [return (csti 1),
    liftM2 (\t c->BinaryExpr Divide (BinaryExpr Multiply c t) c)
        divMulTreeGen (liftM csti $ choose (1,500)),
    liftM2 (\t c->BinaryExpr Multiply (BinaryExpr Divide t c) c)
        divMulTreeGen (liftM csti $ choose (1,500))]

tests :: IO [Test]
tests = return $ [
        Group "numerical" True [
            reduces "natural-num-add"
                (map sumExprs $ permutations $ map csti [0..5])
                (csti $ sum [0..5]),
            reduces "exact-int-sum"
                (map sumExprs $ permutations [csti 2, cstf 5 (-1)])
                (cstf 25 (-1)),
            rewriteTest "exact-zero-sum"
                (fmap (sumExprs . concat) $
                    ((listOf1 $ oneof [
                        fmap (\x->[csti x, csti (-x)]) (choose (0, 500)),
                        fmap (\(d,p)->[cstf d p, cstf (-d) p])
                            (liftM2 (,) (choose (0,5000)) (choose (-50,50)))])
                    >>= Q.shuffle))
                (csti 0),
            rewriteTest "exact-one-mul-div" divMulTreeGen (csti 1),
            reduces' "exact-int-subtraction"
                (BinaryExpr Subtract (Constant (ExactReal 1212 (-3) noUnit))
                                     (Constant (IntValue  1 noUnit)))
                (Constant (ExactReal  212 (-3) noUnit))
        ]
    ]
