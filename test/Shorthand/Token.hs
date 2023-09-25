module Shorthand.Token where

import Shorthand.Location
import Token


_int :: Int -> Token
_int value = Token (Integer value) _location


_plus :: Token
_plus = Token Plus _location
