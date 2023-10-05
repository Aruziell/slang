module Token (Token(..), Value(..)) where

import Location


data Token
    = Token Value Location
    deriving (Eq, Show)


data Value
    = End
    | Equals
    | Identifier String
    | Integer Int
    | ParenthesisLeft
    | ParenthesisRight
    | Plus
    | Separator
    | When | Then | Else
    deriving (Eq, Show)
