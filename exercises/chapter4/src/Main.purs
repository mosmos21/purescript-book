module Main where

import Prelude

import Control.Alternative (guard)
import Data.Array (concatMap, (..), filter)
import Data.Foldable (product)
import Effect (Effect)
import Effect.Console (log)

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