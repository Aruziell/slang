module Tokenizer (TokenizeError(..), tokenize) where

import Data.Char (isDigit)

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
tokenize_ loc (' ' : rest) = tokenize_ (advance loc) rest
tokenize_ loc ('+' : rest) = ([T.Token T.Plus loc] ++) <$> tokenize_ (advance loc) rest
tokenize_ loc text@(c:_)
    | isDigit c =
        let valueString = takeWhile isDigit text
            len = length valueString
            value = tokenizeInteger valueString
        in ([T.Token value loc] ++) <$> tokenize_ (advanceBy len loc) (drop len text)
tokenize_ loc (c:_) = Left (IllegalCharacter c loc)


tokenizeInteger :: String -> T.Value
tokenizeInteger text =
    T.Integer (read text :: Int)


startLocation :: L.Location
startLocation = L.Location { L.line = 0, L.column = 0 }


advance :: L.Location -> L.Location
advance = advanceBy 1


advanceBy :: Int -> L.Location -> L.Location
advanceBy cols loc = L.Location (L.line loc) (L.column loc + cols)
