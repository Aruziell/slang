module Token (Token(..), Value(..)) where

import Location


data Token
    = Token Value Location
    deriving (Eq, Show)


data Value
    = Begin | End
    | Equals
    | GreaterThan
    | Identifier String
    | Integer Int
    | Minus
    | Newline
    | ParenthesisLeft
    | ParenthesisRight
    | Plus
    | Separator
    | When | Then | Else
    deriving (Eq, Show)
