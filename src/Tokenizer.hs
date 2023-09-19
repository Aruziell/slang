module Tokenizer (tokenize) where

import qualified Token as T


tokenize :: String -> [T.Token]
tokenize text = map tokenizeWord (words text)


tokenizeWord :: String -> T.Token
tokenizeWord word@('+':_) = tokenizePlus word
tokenizeWord word = tokenizeInteger word


tokenizeInteger :: String -> T.Token
tokenizeInteger text =
    T.IntegerToken (read text :: Int)


tokenizePlus :: String -> T.Token
tokenizePlus ['+'] =
    T.PlusToken
