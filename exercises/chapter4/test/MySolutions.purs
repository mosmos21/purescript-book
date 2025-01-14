module Test.MySolutions where

import Data.Path
import Prelude

import Control.Alternative (guard)
import Data.Array (concatMap, cons, filter, foldl, head, length, null, tail, (..))
import Data.Maybe (Maybe(..), fromMaybe)
import Main (factors)
import Test.Examples (allFiles)

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

keepNonNegative :: Array Number -> Array Number
keepNonNegative = filter (\x -> x >= 0.0)

infix 8 filter as <$?>

keepNonNegativeRewrite :: Array Number -> Array Number
keepNonNegativeRewrite arr = (\x -> x >= 0.0) <$?> arr

isPrime :: Int -> Boolean
isPrime n = case n of 
    0 -> false
    1 -> false 
    _ -> eq 1 $ length $ factors n

cartesianProduct :: forall a . Array a -> Array a -> Array (Array a)
cartesianProduct xs ys = do
    x <- xs
    y <- ys
    pure [x, y]

triples :: Int -> Array (Array Int)
triples n = do
    x <- 1 .. n
    y <- x .. n
    z <- y .. n
    guard $ x * x + y * y == z * z
    pure [x, y, z]

primeFactors :: Int -> Array Int
primeFactors n = case n of
    0 -> []
    1 -> []
    _ -> devide 2 n
    where
        devide :: Int -> Int -> Array Int
        devide d x = case x of
            1 -> []
            _ | x `mod` d == 0 -> cons d $ devide d (x / d)
            _ -> devide (d + 1) x

allTrue :: Array Boolean -> Boolean
allTrue = foldl (&&) true

fibTailRec :: Int -> Int
fibTailRec = fib' 0 1
  where
  fib' :: Int -> Int -> Int -> Int
  fib' a b n' =
    if n' == 0 then
      a
    else
      fib' b (a + b) (n' - 1) 

reverse :: forall a . Array a -> Array a
reverse = foldl (flip cons) []

isFile :: Path -> Boolean
isFile (Directory _ _) = false
isFile (File _ _) = true

onlyFiles :: Path -> Array Path
onlyFiles path = if isFile path
    then [path]
    else concatMap onlyFiles $ ls path

whereIs :: Path -> String -> Maybe Path
whereIs path name = head $ do
    path' <- allFiles path
    path'' <- ls path'
    guard $ filename path'' == filename path' <> name
    pure path'

largestSmallest :: Path -> Array Path
largestSmallest path = foldl select [] $ onlyFiles path
    where
        select :: Array Path -> Path -> Array Path
        select [file] current   | size current > size file = [current, file]
                                | otherwise = [file, current]
        select [largest, smallest] current  | size current < size largest = [current, smallest]
                                            | size smallest < size current = [largest, current]
                                            | otherwise = [largest, smallest]
        select _ current = [current]