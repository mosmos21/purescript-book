module Test.MySolutions where

import Prelude
import Data.Person

factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial (n - 1)

binomial :: Int -> Int -> Int
binomial _ 0 = 1
binomial 0 _ = 0
binomial n k    | k > n = 0
                | otherwise = (factorial n) / (factorial k * factorial (n - k))

pascal :: Int -> Int -> Int
pascal _ 0 = 1
pascal 0 _ = 0
pascal n k = pascal (n - 1) k + pascal (n - 1) (k - 1)

sameCity :: Person -> Person -> Boolean
sameCity { address: { city: c1 } } { address: { city: c2 } } = c1 == c2

fromSingleton :: forall a . a -> Array a -> a
fromSingleton _ [x] = x
fromSingleton d _ = d
