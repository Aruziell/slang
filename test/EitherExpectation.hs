module EitherExpectation (shouldBeRight) where

import Test.Hspec


shouldBeRight :: (Eq a, Show a, Eq b, Show b) => Either a b -> b -> Expectation
shouldBeRight actual expected = actual `shouldBe` Right expected
