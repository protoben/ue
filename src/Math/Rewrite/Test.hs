module Math.Rewrite.Test (tests) where

import Test.QuickCheck
import Distribution.TestSuite

--satisfy :: Testable p => String -> p -> Test
--satisfy tname p = Test $ 

tests :: IO [Test]
tests = return $ map Test [ ]
