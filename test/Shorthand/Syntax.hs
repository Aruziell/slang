module Shorthand.Syntax where

import Shorthand.Location
import Syntax


_int :: Int -> Expression
_int value = int value _location


_def :: String -> Expression -> Definition
_def = Definition _location


_plus :: Expression -> Expression -> Expression
_plus l r = (l `plus` r) _location
