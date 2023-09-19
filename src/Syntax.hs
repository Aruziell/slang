module Syntax (Expression(..), Program(..)) where


data Program
    = Program Expression
    deriving (Eq, Show)


data Expression
    = IntegerLiteral Int
    | PlusOperator Expression Expression
    deriving (Eq, Show)
