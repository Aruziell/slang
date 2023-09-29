module Tokenizer (TokenizeError(..), tokenize) where

import Data.Char (isDigit, isLetter)
import qualified Data.List.NonEmpty as NonEmpty (NonEmpty(..), drop, takeWhile)

import qualified Location as L (Location(..))
import qualified Token as T


data TokenizeError
    = IllegalCharacter Char L.Location
    deriving (Eq, Show)


type TokenizeResult = Either TokenizeError [T.Token]
type Tokenizer = String -> TokenizeResult


tokenize :: Tokenizer
tokenize = tokenize_ startLocation


tokenize_ :: L.Location -> Tokenizer
tokenize_ _ [] = Right []
tokenize_ loc (' ' : rest) =
    tokenize_ (advance loc) rest
tokenize_ loc ('\n' : rest) =
    ([T.Token T.End loc] ++) <$> tokenize_ (advance loc) rest
tokenize_ loc ('=' : rest) =
    ([T.Token T.Equals loc] ++) <$> tokenize_ (advance loc) rest
tokenize_ loc ('+' : rest) =
    ([T.Token T.Plus loc] ++) <$> tokenize_ (advance loc) rest
tokenize_ loc text@(c:cs)
    | isDigit c =
        let valueString = takeWhile isDigit text
            len = length valueString
            value = tokenizeInteger valueString
        in ([T.Token value loc] ++) <$> tokenize_ (advanceBy len loc) (drop len text)
    | isLetter c = do
        (value, len, rest) <- tokenizeIdentifier loc (c NonEmpty.:| cs)
        ([T.Token value loc] ++) <$> tokenize_ (advanceBy len loc) rest
tokenize_ loc (c:_) = Left (IllegalCharacter c loc)


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


startLocation :: L.Location
startLocation = L.Location { L.line = 0, L.column = 0 }


advance :: L.Location -> L.Location
advance = advanceBy 1


advanceBy :: Int -> L.Location -> L.Location
advanceBy cols loc = L.Location (L.line loc) (L.column loc + cols)
