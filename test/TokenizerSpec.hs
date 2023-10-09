module TokenizerSpec (spec) where

import Test.Hspec

import Tokenizer
import qualified Location as L
import qualified Token as T

import EitherExpectation (shouldBeRight)


spec :: Spec
spec = do
    it "empty" $ do
        tokenize "" `shouldBeRight` []

    it "skips spaces" $ do
        tokenize "   " `shouldBeRight` []

    it "indent" $ do
        tokenize "    "
        `shouldBeRight`
            [ T.Token (T.Begin) (L.Location 0 0)
            , T.Token (T.End) (L.Location 0 4)
            ]

    it "triple indent" $ do
        tokenize "            "
        `shouldBeRight`
            [ T.Token (T.Begin) (L.Location 0 0)
            , T.Token (T.Begin) (L.Location 0 4)
            , T.Token (T.Begin) (L.Location 0 8)
            , T.Token (T.End) (L.Location 0 12)
            , T.Token (T.End) (L.Location 0 12)
            , T.Token (T.End) (L.Location 0 12)
            ]

    it "dedent" $ do
        (ignoreNewlines <$>) . tokenize $ unlines
            [ "        "
            , "    "
            ]
        `shouldBeRight`
            [ T.Token (T.Begin) (L.Location 0 0)
            , T.Token (T.Begin) (L.Location 0 4)
            , T.Token (T.End) (L.Location 1 4)
            , T.Token (T.End) (L.Location 2 0)
            ]

    it "indented block" $ do
        (ignoreNewlines <$>) . tokenize $ unlines
            [ "block"
            , "    foo"
            , "    1"
            , "        bar"
            ]
        `shouldBeRight`
            [ T.Token (T.Identifier "block") (L.Location 0 0)
            , T.Token (T.Begin) (L.Location 1 0)
                , T.Token (T.Identifier "foo") (L.Location 1 4)
                , T.Token (T.Integer 1) (L.Location 2 4)
                , T.Token (T.Begin) (L.Location 3 4)
                    , T.Token (T.Identifier "bar") (L.Location 3 8)
                , T.Token (T.End) (L.Location 4 0)
            , T.Token (T.End) (L.Location 4 0)
            ]

    it "single-digit integer" $ do
        tokenize "1" `shouldBeRight` [T.Token (T.Integer 1) (L.Location 0 0)]

    it "multiple-digit integer" $ do
        tokenize "1000" `shouldBeRight` [T.Token (T.Integer 1000) (L.Location 0 0)]

    it "parenthesized integer" $ do
        tokenize "(1)"
        `shouldBeRight`
            [ T.Token T.ParenthesisLeft     (L.Location 0 0)
            , T.Token (T.Integer 1)         (L.Location 0 1)
            , T.Token T.ParenthesisRight    (L.Location 0 2)
            ]
    
    it "parenthesized sum" $ do
        tokenize "(1 + 2)"
        `shouldBeRight`
            [ T.Token T.ParenthesisLeft     (L.Location 0 0)
            , T.Token (T.Integer 1)         (L.Location 0 1)
            , T.Token T.Plus                (L.Location 0 3)
            , T.Token (T.Integer 2)         (L.Location 0 5)
            , T.Token T.ParenthesisRight    (L.Location 0 6)
            ]
    
    it "separator" $ do
        tokenize ","
        `shouldBeRight` [T.Token T.Separator (L.Location 0 0)]

    it "equals" $ do
        tokenize "=" `shouldBeRight` [T.Token T.Equals (L.Location 0 0)]

    it "plus operator" $ do
        tokenize "+" `shouldBeRight` [T.Token T.Plus (L.Location 0 0)]
    
    it "identifier" $ do
        tokenize "foo"
        `shouldBeRight`
            [ T.Token (T.Identifier "foo") (L.Location 0 0) ]

    it "integer addition" $ do
        tokenize "1 + 2" `shouldBeRight`
            [ T.Token (T.Integer 1) (L.Location 0 0)
            , T.Token T.Plus (L.Location 0 2)
            , T.Token (T.Integer 2) (L.Location 0 4)
            ]

    it "prefix integer addition" $ do
        tokenize "+ 1 2" `shouldBeRight`
            [ T.Token T.Plus (L.Location 0 0)
            , T.Token (T.Integer 1) (L.Location 0 2)
            , T.Token (T.Integer 2) (L.Location 0 4)
            ]

    it "postfix integer addition" $ do
        tokenize "1 2 +" `shouldBeRight`
            [ T.Token (T.Integer 1) (L.Location 0 0)
            , T.Token (T.Integer 2) (L.Location 0 2)
            , T.Token T.Plus (L.Location 0 4)
            ]

    it "integer column location" $ do
        tokenize "1 1  1   1"
        `shouldBeRight`
            [ T.Token (T.Integer 1) (L.Location 0 0)
            , T.Token (T.Integer 1) (L.Location 0 2)
            , T.Token (T.Integer 1) (L.Location 0 5)
            , T.Token (T.Integer 1) (L.Location 0 9)
            ]

    describe "condition" $ do
    
        it "when" $
            tokenize "when"
            `shouldBeRight` [T.Token T.When (L.Location 0 0)]
        
        it "then" $
            tokenize "then"
            `shouldBeRight` [T.Token T.Then (L.Location 0 0)]

        it "else" $
            tokenize "else"
            `shouldBeRight` [T.Token T.Else (L.Location 0 0)]

    illegalCharacter '~'
    illegalCharacter '!'
    illegalCharacter '%'


ignoreNewlines :: [T.Token] -> [T.Token]
ignoreNewlines = filter (\(T.Token value _) -> value /= T.Newline)


illegalCharacter :: Char -> SpecWith (Arg Expectation)
illegalCharacter c = it ("yields IllegalCharacter " ++ [c]) $
    tokenize [c] `shouldBe` Left (IllegalCharacter c (L.Location 0 0))
