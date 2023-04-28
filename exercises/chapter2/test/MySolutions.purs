module Test.MySolutions where

import Prelude
import Data.Number (sqrt)

diagonal :: Number -> Number -> Number
diagonal n p = sqrt $ n * n + p * p
