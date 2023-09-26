module ParserSpec (spec) where

import Test.Hspec

import Parser
import qualified Location as L
import qualified Shorthand.Syntax as S
import qualified Shorthand.Token as T
import qualified Syntax as S
import qualified Token as T

import EitherExpectation


spec :: Spec
spec = do
    it "integer" $
        parseExpression [T._int 1]
            `shouldBeRight` S._int 1

    it "integer addition" $
        parseExpression
            [ T._int 1
            , T._plus
            , T._int 2
            ]
        `shouldBeRight`
            (S._int 1 `S._plus` S._int 2)

    it "definition" $
        parse
            [ T._id "foo", T._eq, T._int 1 ]
        `shouldBeRight` S.Program
            (S._def "foo" (S._int 1))

    it "three integer addition" $
        parse
            [ T._id "foo"
            , T._eq
            , T._int 1
            , T._plus
            , T._int 2
            , T._plus
            , T._int 3
            ]
        `shouldBeRight` S.Program
            (S._def "foo" $ S._int 1 `S._plus` (S._int 2 `S._plus` S._int 3))

    it "missing left add operand" $ do
        parseExpression [T._plus, T._int 1]
            `shouldBe` Left IncompleteExpression

    it "missing right add operand" $ do
        parseExpression [T._int 1, T._plus]
            `shouldBe` Left IncompleteExpression

    it "missing both add operands" $ do
        parseExpression [T._plus] `shouldBe` Left IncompleteExpression

    it "preserves location" $ do
        parseExpression [T.Token (T.Integer 1) location]
        `shouldBeRight` S.Expression (S.IntegerLiteral 1) location
        where location = L.Location 3 5
