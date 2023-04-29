module Test.MySolutions where

import Prelude
import Data.Array (head, tail, null)
import Data.Maybe (Maybe(..), fromMaybe)

-- Note to reader: Add your solutions to this file
isEven :: Int -> Boolean
isEven x = case x of
    0 -> true
    1 -> false
    _ | x < 0 -> isEven (-x)
    _ -> isEven (x - 2)

oneIfJustEven :: Maybe Int -> Int
oneIfJustEven x = case x of
    Nothing -> 0
    Just x -> if isEven x then 1 else 0

countEven :: Array Int -> Int
countEven xs = if null xs
    then 0
    else (oneIfJustEven $ head xs) + (countEven $ fromMaybe [] $ tail xs)

squared :: Array Number -> Array Number
squared = map (\x -> x * x)