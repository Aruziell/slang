module Syntax where

import Location (Location)


data Program
    = Program Main [Definition]
    deriving (Eq, Show)


data Expression
    = Expression ExpressionValue Location
    deriving (Eq, Show)


data ExpressionValue
    = Literal LiteralValue
    | PlusOperator Expression Expression
    deriving (Eq, Show)


data LiteralValue
    = Integer Int
    | Identifier String
    deriving (Eq, Show)


data Definition
    = Definition Location String Expression
    deriving (Eq, Show)


data Main
    = Main Location Expression
    deriving (Eq, Show)


identifier :: String -> Location -> Expression
identifier name = Expression (Literal (Identifier name))


int :: Int -> Location -> Expression
int value = Expression (Literal (Integer value))


plus :: Expression -> Expression -> Location -> Expression
plus l r = Expression (l `PlusOperator` r)
