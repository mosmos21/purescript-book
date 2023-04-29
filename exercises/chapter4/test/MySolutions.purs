module Test.MySolutions where

import Prelude

-- Note to reader: Add your solutions to this file
isEven :: Int -> Boolean
isEven x = case x of
    0 -> true
    1 -> false
    _ | x < 0 -> isEven (-x)
    _ -> isEven (x - 2)
