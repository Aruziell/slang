module Tokenizer (Tokenizer, TokenizeError(..), tokenize) where

import Data.Char (isDigit, isLetter)
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


tokenize_ :: TokenizeContext -> Tokenizer
tokenize_ context []
    | (indent context) == 0 = Right []
    | otherwise = (token context T.End :) <$> tokenize_ (dedent context) []
tokenize_ context (' ' : rest) =
    tokenize_ (advance context) rest
tokenize_ context ('\n' : rest) =
    (token context T.Newline :) <$> tokenizeIndent (advanceLine context) rest
tokenize_ context ('(' : rest) =
    (token context T.ParenthesisLeft :) <$> tokenize_ (advance context) rest
tokenize_ context (')' : rest) =
    (token context T.ParenthesisRight :) <$> tokenize_ (advance context) rest
tokenize_ context (',' : rest) =
    (token context T.Separator :) <$> tokenize_ (advance context) rest
tokenize_ context ('=' : rest) =
    (token context T.Equals :) <$> tokenize_ (advance context) rest
tokenize_ context ('+' : rest) =
    (token context T.Plus :) <$> tokenize_ (advance context) rest
tokenize_ context ('-' : rest) =
    (token context T.Minus :) <$> tokenize_ (advance context) rest
tokenize_ context ('>' : rest) =
    (token context T.GreaterThan :) <$> tokenize_ (advance context) rest
tokenize_ context ('w':'h':'e':'n' : rest) =
    (token context T.When :) <$> tokenize_ (advance context) rest
tokenize_ context ('t':'h':'e':'n' : rest) =
    (token context T.Then :) <$> tokenize_ (advance context) rest
tokenize_ context ('e':'l':'s':'e' : rest) =
    (token context T.Else :) <$> tokenize_ (advance context) rest
tokenize_ context text@(c:cs)
    | isDigit c =
        let valueString = takeWhile isDigit text
            len = length valueString
            value = tokenizeInteger valueString
        in (token context value :) <$> tokenize_ (advanceBy len context) (drop len text)
    | isLetter c = do
        (value, len, rest) <- tokenizeIdentifier (toLocation context) (c NonEmpty.:| cs)
        ([T.Token value (toLocation context)] ++) <$> tokenize_ (advanceBy len context) rest
tokenize_ context (c:_) = Left (IllegalCharacter c (toLocation context))


tokenizeIndent :: TokenizeContext -> Tokenizer
tokenizeIndent context = tokenizeIndent_ 0 context


tokenizeIndent_ :: Int -> TokenizeContext -> Tokenizer
tokenizeIndent_ current context (' ':' ':' ':' ' : rest)
    | current < (indent context) = tokenizeIndent_ (current + 1) (advanceBy 4 context) rest
    | otherwise = (token context T.Begin :) <$> tokenizeIndent_ (current + 1) (indent_ context) rest
tokenizeIndent_ current context rest
    | current < (indent context) = (token context T.End :) <$> tokenizeIndent_ current (dedent context) rest
    | otherwise = tokenize_ context rest


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
