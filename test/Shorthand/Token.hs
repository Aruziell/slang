module Shorthand.Token where

import Shorthand.Location
import Token


_id :: String -> Token
_id value = Token (Identifier value) _location


_int :: Int -> Token
_int value = Token (Integer value) _location


_eq :: Token
_eq = Token Equals _location 


_plus :: Token
_plus = Token Plus _location
