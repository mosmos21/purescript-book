module Main where

import Prelude

import Control.Alternative (guard)
import Data.Array (concatMap, (..), filter, null, tail)
import Data.Foldable (product, foldr)
import Effect (Effect)
import Effect.Console (log)
import Data.Maybe (fromMaybe)

main :: Effect Unit
main = do
  log "🍝"

pairs :: Int -> Array (Array Int)
pairs n = concatMap (\x -> map (\y -> [x, y]) $ x .. n) $ 1 .. n

factors :: Int -> Array (Array Int)
factors n = filter (\pair -> product pair == n) $ pairs n

factors2 :: Int -> Array (Array Int)
factors2 n = filter (\xs -> product xs == n) do
  x <- 1 .. n
  y <- x .. n
  pure [x, y]

factors3 :: Int -> Array (Array Int)
factors3 n = do
  x <- 1 .. n
  y <- x .. n
  guard $ x * y == n
  pure [x, y]


lengthTailRec ∷ forall a . Array a → Int
lengthTailRec arr = length' arr 0
  where
  length' :: Array a -> Int -> Int
  length' arr' acc = if null arr'
    then acc
    else length' (fromMaybe [] $ tail arr') (acc + 1)

reverse' :: forall a . Array a -> Array a
reverse' = foldr (\x acc -> acc <> [x]) [] 

fib :: Int -> Int
fib n =
  if n == 0 then
    0
  else if n == 1 then
    1
  else
    fib (n - 1) + fib (n - 2)
