module Util.Aoc2023.Array where

import Data.Array.IArray
import Data.List (find, transpose)
import Util.Aoc2023.Vec2

array2 :: [[a]] -> Array (Vec2 Int) a
array2 vss = listArray (Vec2 0 0, Vec2 maxX maxY) $ concat $ transpose vss
  where
    maxY = length vss - 1
    maxX = foldl ((. length) . max) 1 vss - 1

findArrIndex :: (IArray a e, Ix i) => (e -> Bool) -> a i e -> Maybe i
findArrIndex f = (fst <$>) . find (f . snd) . assocs

-- not in 0.5.4.0 for some reason, bleh
(!?) :: (IArray a e, Ix i) => a i e -> i -> Maybe e
a !? i = if inRange (bounds a) i then Just (a ! i) else Nothing
