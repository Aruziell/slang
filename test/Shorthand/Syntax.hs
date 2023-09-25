module Shorthand.Syntax where

import Shorthand.Location
import qualified Syntax as S


_int :: Int -> S.Expression
_int value = S.int value _location


_plus :: S.Expression -> S.Expression -> S.Expression
_plus l r = (l `S.plus` r) _location
