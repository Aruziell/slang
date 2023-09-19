module ParserSpec (spec) where

import Test.Hspec

import Parser
import qualified Syntax as S
import qualified Token as T


spec :: Spec
spec = do
    it "integer" $ do
        parse [T.IntegerToken 1] `shouldBe` S.Program (S.IntegerLiteral 1)

    it "integer addition" $ do
        parse [T.IntegerToken 1, T.PlusToken, T.IntegerToken 2]
            `shouldBe` S.Program (
                S.PlusOperator (S.IntegerLiteral 1) (S.IntegerLiteral 2)
            )

    it "three integer addition" $ do
        parse
            [ T.IntegerToken 1
            , T.PlusToken
            , T.IntegerToken 2
            , T.PlusToken
            , T.IntegerToken 3
            ]
        `shouldBe` S.Program (
            S.PlusOperator (S.IntegerLiteral 1) (
                S.PlusOperator (S.IntegerLiteral 2) (S.IntegerLiteral 3)
            )
        )
