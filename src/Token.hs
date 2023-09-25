module Token (Token(..), Value(..)) where

import Location


data Token
    = Token Value Location
    deriving (Eq, Show)


data Value
    = Integer Int
    | Plus
    deriving (Eq, Show)
