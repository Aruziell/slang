module Tokenizer (tokenize) where

import qualified Token as T


tokenize :: String -> T.Token
tokenize = tokenizeInteger


tokenizeInteger :: String -> T.Token
tokenizeInteger text =
    T.IntegerToken (read text :: Int)
