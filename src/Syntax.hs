module Syntax
    ( Definition(..)
    , Expression(..)
    , ExpressionValue(..)
    , Main(..)
    , Program(..)
    , int
    , plus
    ) where

import Location (Location)


data Program
    = Program Main [Definition]
    deriving (Eq, Show)


data Expression
    = Expression ExpressionValue Location
    deriving (Eq, Show)


data ExpressionValue
    = IntegerLiteral Int
    | PlusOperator Expression Expression
    deriving (Eq, Show)


data Definition
    = Definition Location String Expression
    deriving (Eq, Show)


data Main
    = Main Location Expression
    deriving (Eq, Show)


int :: Int -> Location -> Expression
int value loc = Expression (IntegerLiteral value) loc


plus :: Expression -> Expression -> Location -> Expression
plus l r loc =
    Expression (l `PlusOperator` r) loc
