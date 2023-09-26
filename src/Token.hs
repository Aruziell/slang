module Token (Token(..), Value(..)) where

import Location


data Token
    = Token Value Location
    deriving (Eq, Show)


data Value
    = Identifier String
    | Integer Int
    | Equals
    | Plus
    deriving (Eq, Show)
