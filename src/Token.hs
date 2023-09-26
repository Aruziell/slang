module Token (Token(..), Value(..)) where

import Location


data Token
    = Token Value Location
    deriving (Eq, Show)


data Value
    = Integer Int
    | Plus
    | Identifier String
    deriving (Eq, Show)
