module Syntax where

import Location (Location)


data Program
    = Program Main [Function]
    deriving (Eq, Show)


data Expression
    = Expression ExpressionValue Location
    deriving (Eq, Show)


data ExpressionValue
    = Literal LiteralValue
    | FunctionCall String [Expression]
    | PlusOperator Expression Expression
    deriving (Eq, Show)


data LiteralValue
    = Integer Int
    deriving (Eq, Show)


data Main
    = Main Location Expression
    deriving (Eq, Show)


data Function
    = Function Location String ArgumentList Expression
    deriving (Eq, Show)


type ArgumentList = [Argument]


data Argument =
    Argument Location String
    deriving (Eq, Show)


call :: String -> [Expression] -> Location -> Expression
call name exprList = Expression (FunctionCall name exprList)


int :: Int -> Location -> Expression
int value = Expression (Literal (Integer value))


plus :: Expression -> Expression -> Location -> Expression
plus l r = Expression (l `PlusOperator` r)
