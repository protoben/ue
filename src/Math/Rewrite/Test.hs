module Math.Rewrite.Test (tests) where

import Data.Expression
import Data.Units
import Math.Rewrite

import Data.List
import Test.QuickCheck
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

-- generate more complex alternate forms for the given expr
--genAlternate :: Expr -> Gen Expr

-- produce a constant integer
csti :: Integer -> Expr
csti = Constant . (\x->IntValue x noUnit)

-- generate a summation of the passed exprs
sumExprs :: [Expr] -> Expr
sumExprs [x] = x
sumExprs (x:xs) = BinaryExpr Add x $ sumExprs xs

tests :: IO [Test]
tests = return $ [
        Group "numerical" True [
            Group "addition" True [
                reduces "natural-nums"
                    (map sumExprs $ permutations $ map csti [0..5])
                    (csti $ sum [0..5])
            ]
        ]
    ]
