module Shorthand.Syntax where

import Shorthand.Location
import Syntax


_arg :: String -> Argument
_arg = Argument _location


_call :: String -> [Expression] -> Expression
_call name args = Expression (FunctionCall name args) _location


_gt :: Expression -> Expression -> Expression
_gt l r = Expression (Logical (GreaterThan _location) l r) _location


_int :: Int -> Expression
_int value = int value _location


_fn :: String -> ArgumentList -> Expression -> Function
_fn = Function _location


_minus :: Expression -> Expression -> Expression
_minus l r = (l `minus` r) _location


_paren :: Expression -> Expression
_paren expr = Expression (Parenthesized expr) _location


_plus :: Expression -> Expression -> Expression
_plus l r = (l `plus` r) _location


_when :: Expression -> [WhenCase] -> Expression -> Expression
_when expr cases else_ = Expression (When expr cases else_) _location
