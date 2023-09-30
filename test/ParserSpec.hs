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
            `shouldBeRight` (S._int 1, [])

    it "integer addition" $
        parseExpression
            [ T._int 1
            , T._plus
            , T._int 2
            ]
        `shouldBeRight`
            ((S._int 1 `S._plus` S._int 2), [])

    it "function" $
        parseFunction
            [ T._id "foo", T._eq, T._int 1, T._end ]
        `shouldBeRight`
            (S._fn "foo" [] (S._int 1), [])

    it "function list" $
        parseFunctionList
            [ T._id "foo", T._eq, T._int 1, T._end
            , T._id "bar", T._eq, T._int 2, T._end
            ]
        `shouldBeRight`
            [ S._fn "foo" [] (S._int 1)
            , S._fn "bar" [] (S._int 2)
            ]

    it "function with a parameter" $
        parseFunction [T._id "foo", T._id "bar", T._eq, T._int 1, T._end]
        `shouldBeRight` (S._fn "foo" [S._arg "bar"] (S._int 1), [])

    it "identifier expression" $ do
        parseExpression [T._id "foo"]
        `shouldBeRight` (S._id "foo", [])

    it "three integer addition" $
        parseExpression
            [ T._int 1
            , T._plus
            , T._int 2
            , T._plus
            , T._int 3
            ]
        `shouldBeRight`
            (S._int 1 `S._plus` (S._int 2 `S._plus` S._int 3), [])

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
        `shouldBeRight` (S.Expression (S.Literal (S.Integer 1)) location, [])
        where location = L.Location 3 5
