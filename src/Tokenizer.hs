module Tokenizer (TokenizeError(..), tokenize) where

import Data.Char (isDigit)

import qualified Token as T


data TokenizeError
    = IllegalCharacter Char
    deriving (Eq, Show)


type TokenizeResult = Either TokenizeError [T.Token]
type Tokenizer = String -> TokenizeResult


tokenize :: Tokenizer
tokenize = tokenize_ startLocation


tokenize_ :: T.Location -> Tokenizer
tokenize_ _ [] = Right []
tokenize_ loc (' ' : rest) = tokenize_ (advance loc) rest
tokenize_ loc ('+' : rest) = ([T.Token T.Plus loc] ++) <$> tokenize_ (advance loc) rest
tokenize_ loc text@(c:_)
    | isDigit c =
        let valueString = takeWhile isDigit text
            len = length valueString
            value = tokenizeInteger valueString
        in ([T.Token value loc] ++) <$> tokenize_ (advanceBy len loc) (drop len text)
tokenize_ _ (c:_) = Left (IllegalCharacter c)


tokenizeInteger :: String -> T.Value
tokenizeInteger text =
    T.Integer (read text :: Int)


startLocation :: T.Location
startLocation = T.Location 0 0


advance :: T.Location -> T.Location
advance = advanceBy 1


advanceBy :: Int -> T.Location -> T.Location
advanceBy cols loc = T.Location (T.line loc) (T.column loc + cols)
