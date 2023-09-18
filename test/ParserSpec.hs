module ParserSpec (spec) where

import Test.Hspec

import Parser
import qualified Syntax as S
import qualified Token as T


spec :: Spec
spec = do
    it "integer" $ do
        parse (T.IntegerToken 1) `shouldBe` S.Program (S.IntegerLiteral 1)
