module Tokenizer (Tokenizer, TokenizeError(..), tokenize) where

import Data.Char (isDigit, isLetter)
import Data.List (stripPrefix)
import qualified Data.List.NonEmpty as NonEmpty (NonEmpty(..), drop, takeWhile)

import qualified Location as L (Location(..))
import qualified Token as T


data TokenizeError
    = IllegalCharacter Char L.Location
    deriving (Eq, Show)


type TokenizeResult = Either TokenizeError [T.Token]
type Tokenizer = String -> TokenizeResult


data TokenizeContext
    = TokenizeContext
        { indent :: Int
        , line :: Int
        , column :: Int
        }


toLocation :: TokenizeContext -> L.Location
toLocation context =
    L.Location
        { L.line = line context
        , L.column = column context
        }


advanceBy :: Int -> TokenizeContext -> TokenizeContext
advanceBy n context = context { column = column context + n }


advance :: TokenizeContext -> TokenizeContext
advance = advanceBy 1


advanceLine :: TokenizeContext -> TokenizeContext
advanceLine context = context { line = line context + 1, column = 0 }


indent_ :: TokenizeContext -> TokenizeContext
indent_ context =
    context
        { indent = indent context + 1
        , column = column context + 4
        }

dedent :: TokenizeContext -> TokenizeContext
dedent context =
    context { indent = indent context - 1 }


startContext :: TokenizeContext
startContext = TokenizeContext { indent = 0, line = 0, column = 0 }


token :: TokenizeContext -> T.Value -> T.Token
token context value = T.Token value (toLocation context)


tokenize :: Tokenizer
tokenize = tokenizeIndent startContext


tokenMap =
    [ ("\n", T.Newline)
    , ("(", T.ParenthesisLeft)
    , (")", T.ParenthesisRight)
    , ("=", T.Equals)
    , ("+", T.Plus)
    , ("-", T.Minus)
    , (",", T.Separator)
    , (">", T.GreaterThan)
    , ("when", T.When)
    , ("then", T.Then)
    , ("else", T.Else)
    ]


tokenize_ :: TokenizeContext -> Tokenizer
tokenize_ context []
    | (indent context) == 0 = Right []
    | otherwise = (token context T.End :) <$> tokenize_ (dedent context) []
tokenize_ context (' ' : rest) =
    tokenize_ (advance context) rest
tokenize_ context ('\n' : rest) =
    (token context T.Newline :) <$> tokenizeIndent (advanceLine context) rest
tokenize_ context text =
    tokenizeFromMap context tokenMap text


tokenizeIndent :: TokenizeContext -> Tokenizer
tokenizeIndent context = tokenizeIndent_ 0 context


tokenizeIndent_ :: Int -> TokenizeContext -> Tokenizer
tokenizeIndent_ current context (' ':' ':' ':' ' : rest)
    | current < (indent context) = tokenizeIndent_ (current + 1) (advanceBy 4 context) rest
    | otherwise = (token context T.Begin :) <$> tokenizeIndent_ (current + 1) (indent_ context) rest
tokenizeIndent_ current context rest
    | current < (indent context) = (token context T.End :) <$> tokenizeIndent_ current (dedent context) rest
    | otherwise = tokenize_ context rest


tokenizeFromMap :: TokenizeContext -> [(String, T.Value)] -> Tokenizer
tokenizeFromMap _ _ [] = return []
tokenizeFromMap context d text = do
    case findPrefix d text of
        Just (prefix, value, rest) -> do
            let len = length prefix
            restTokens <- tokenize_ (advanceBy len context) rest
            Right $ token context value : restTokens
        Nothing -> tokenizeIdentifierOrLiteral context text


tokenizeIdentifierOrLiteral :: TokenizeContext -> Tokenizer
tokenizeIdentifierOrLiteral context text@(c:cs)
    | isDigit c =
        let valueString = takeWhile isDigit text
            len = length valueString
            value = tokenizeInteger valueString
        in (token context value :) <$> tokenize_ (advanceBy len context) (drop len text)
    | isLetter c = do
        (value, len, rest) <- tokenizeIdentifier (toLocation context) (c NonEmpty.:| cs)
        ([T.Token value (toLocation context)] ++) <$> tokenize_ (advanceBy len context) rest
    | otherwise = Left (IllegalCharacter c (toLocation context))


tokenizeInteger :: String -> T.Value
tokenizeInteger text =
    T.Integer (read text :: Int)


tokenizeIdentifier :: L.Location -> NonEmpty.NonEmpty Char -> Either TokenizeError (T.Value, Int, String)
tokenizeIdentifier loc text@(c NonEmpty.:| _)
    | isLetter c =
        let value = NonEmpty.takeWhile isLetter text
            len = length value
            rest = NonEmpty.drop len text
        in Right (T.Identifier value, len, rest)
    | otherwise =
        Left (IllegalCharacter c loc)


findPrefix :: [(String, a)] -> String -> Maybe (String, a, String)
findPrefix [] text = Nothing
findPrefix ((prefix, value) : d) text = do
    case stripPrefix prefix text of
        Just rest -> Just (prefix, value, rest)
        Nothing -> findPrefix d text
