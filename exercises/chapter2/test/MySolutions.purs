module Test.MySolutions where

import Prelude
import Data.Number (sqrt, pi)

diagonal :: Number -> Number -> Number
diagonal n p = sqrt $ n * n + p * p

circleArea :: Number -> Number
circleArea r = pi * r * r
