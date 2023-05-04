module Test.MySolutions where

import Data.Person
import Data.Picture
import Prelude

import Data.Maybe (Maybe(..))

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

circleAtOrigin :: Shape
circleAtOrigin = Circle center 10.0
    where 
        center :: Point
        center = { x: 0.0, y: 0.0 }

doubleScaleAndCenter :: Shape -> Shape
doubleScaleAndCenter (Circle _ r) = Circle { x: 0.0, y: 0.0 } (r * 2.0)
doubleScaleAndCenter (Rectangle _ w h) = Rectangle { x: 0.0, y: 0.0 } (w * 2.0) (h * 2.0)
doubleScaleAndCenter (Line p1 p2) = Line { x: p1'.x * 2.0, y: p1'.y * 2.0 }  { x: p2'.x * 2.0, y: p2'.y * 2.0 }
    where
      center = { x: (p1.x + p2.x) / 2.0, y: (p1.y + p2.y) / 2.0 }
      p1' = { x: p1.x - center.x, y: p1.y - center.y }
      p2' = { x: p2.x - center.x, y: p2.y - center.y }
doubleScaleAndCenter (Text _ str) = Text { x: 0.0, y: 0.0 } str

shapeText:: Shape -> Maybe String
shapeText (Text _ str) = Just str
shapeText _ = Nothing