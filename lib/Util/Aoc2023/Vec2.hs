module Util.Aoc2023.Vec2 where

import Data.Array.IArray (Ix, inRange, index, range, rangeSize)
import Data.Ord (comparing)

data Vec2 n = Vec2 {x :: n, y :: n} deriving (Show, Eq, Functor)

fromPair :: (n, n) -> Vec2 n
fromPair (x, y) = Vec2 {x, y}

instance (Num n) => Num (Vec2 n) where
  Vec2 a b + Vec2 c d = Vec2 (a + c) (b + d)
  Vec2 a b * Vec2 c d = Vec2 (a * c) (b * d)
  Vec2 a b - Vec2 c d = Vec2 (a - c) (b - d)
  abs = fmap abs
  signum = fmap signum
  fromInteger i = Vec2 (fromInteger i) (fromInteger i)

instance (Ix n) => Ord (Vec2 n) where
  compare = comparing y <> comparing x

instance (Ix n) => Ix (Vec2 n) where
  range (Vec2 l1 l2, Vec2 u1 u2) =
    [Vec2 i1 i2 | i1 <- range (l1, u1), i2 <- range (l2, u2)]
  index (Vec2 l1 l2, Vec2 u1 u2) (Vec2 i1 i2) =
    index (l1, u1) i1 * rangeSize (l2, u2) + index (l2, u2) i2
  inRange (Vec2 l1 l2, Vec2 u1 u2) (Vec2 i1 i2) =
    inRange (l1, u1) i1 && inRange (l2, u2) i2

instance (Semigroup n) => Semigroup (Vec2 n) where
  Vec2 x1 y1 <> Vec2 x2 y2 = Vec2 (x1 <> x2) (y1 <> y2)

instance (Monoid n) => Monoid (Vec2 n) where
  mempty = Vec2 mempty mempty

mag :: (Real n, Floating f) => Vec2 n -> f
mag = dist $ Vec2 0 0

-- Euclidean distance between two points
dist :: (Real n, Floating f) => Vec2 n -> Vec2 n -> f
dist (Vec2 x1 y1) (Vec2 x2 y2) = sqrt (x' * x' + y' * y')
  where
    x' = realToFrac x2 - realToFrac x1
    y' = realToFrac y2 - realToFrac y1

taxicabDist :: (Num n) => Vec2 n -> Vec2 n -> n
taxicabDist (Vec2 x1 y1) (Vec2 x2 y2) = abs (x1 - x2) + abs (y1 - y2)
