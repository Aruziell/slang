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
    | Parenthesized Expression
    | BinaryOperator Operator Expression Expression
    -- When expr [cases] else
    | When Expression [WhenCase] Expression
    deriving (Eq, Show)


data Operator
    = Add Location
    | Sub Location
    deriving (Eq, Show)


operatorLocation :: Operator -> Location
operatorLocation (Add loc) = loc
operatorLocation (Sub loc) = loc


type WhenCase = (Expression, Expression)


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
plus l r loc = Expression (BinaryOperator (Add loc) l r) loc


minus :: Expression -> Expression -> Location -> Expression
minus l r loc = Expression (BinaryOperator (Sub loc) l r) loc
