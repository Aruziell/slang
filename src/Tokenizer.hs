module Tokenizer (tokenize) where

import Data.Char (isDigit)

import qualified Token as T


tokenize :: String -> [T.Token]
tokenize = tokenize_ startLocation


tokenize_ :: T.Location -> String -> [T.Token]
tokenize_ _ [] = []
tokenize_ loc text@(c:rest)
    | c == ' ' = [] ++ tokenize_ (advance loc) rest
    | c == '+' = [T.Token T.Plus loc] ++ tokenize_ (advance loc) rest
    | isDigit c =
        let intString = takeWhile isDigit text
            len = length intString
            value = tokenizeInteger intString
        in [T.Token value loc] ++ tokenize_ (advanceBy len loc) (drop len text)


tokenizeInteger :: String -> T.Value
tokenizeInteger text =
    T.Integer (read text :: Int)


startLocation :: T.Location
startLocation = T.Location 0 0


advance :: T.Location -> T.Location
advance = advanceBy 1


advanceBy :: Int -> T.Location -> T.Location
advanceBy cols loc = T.Location (T.line loc) (T.column loc + cols)
