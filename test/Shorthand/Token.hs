module Shorthand.Token where

import Shorthand.Location
import Token


_id :: String -> Token
_id value = Token (Identifier value) _location


_int :: Int -> Token
_int value = Token (Integer value) _location


_eq :: Token
_eq = Token Equals _location 


_end :: Token
_end = Token End _location


_when :: Token
_when = Token When _location


_then :: Token
_then = Token Then _location


_pLeft :: Token
_pLeft = Token ParenthesisLeft _location


_pRight :: Token
_pRight = Token ParenthesisRight _location


_plus :: Token
_plus = Token Plus _location


_sep :: Token
_sep = Token Separator _location
