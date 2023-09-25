module Syntax (Expression(..), Program(..), Value(..), int, plus) where

import Location (Location)


data Program
    = Program Expression
    deriving (Eq, Show)


data Expression
    = Expression Value Location
    deriving (Eq, Show)


data Value
    = IntegerLiteral Int
    | PlusOperator Expression Expression
    deriving (Eq, Show)


int :: Int -> Location -> Expression
int value loc = Expression (IntegerLiteral value) loc


plus :: Expression -> Expression -> Location -> Expression
plus l r loc =
    Expression (l `PlusOperator` r) loc
