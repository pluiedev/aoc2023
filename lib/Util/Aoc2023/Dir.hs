module Util.Aoc2023.Dir where

class Dir d where
  -- Get the opposite direction. Equivalent to rotating 180 degrees.
  opposite :: d -> d

  -- Rotate clockwise 90 degrees.
  cw90 :: d -> d

  -- Rotate counterclockwise 90 degrees.
  ccw90 :: d -> d

data Dir4 = North | West | South | East deriving (Show, Eq, Ord)

instance Dir Dir4 where
  opposite North = South
  opposite South = North
  opposite East = West
  opposite West = East

  cw90 North = East
  cw90 East = South
  cw90 South = West
  cw90 West = North

  ccw90 North = West
  ccw90 West = South
  ccw90 South = East
  ccw90 East = North

dir4s :: [Dir4]
dir4s = [North, West, South, East]

data Dir8
  = DueNorth
  | NorthWest
  | DueWest
  | SouthWest
  | DueSouth
  | SouthEast
  | DueEast
  | NorthEast
  deriving (Show, Eq)

instance Dir Dir8 where
  opposite DueNorth = DueSouth
  opposite NorthWest = SouthEast
  opposite DueWest = DueEast
  opposite SouthWest = NorthEast
  opposite DueSouth = DueNorth
  opposite SouthEast = NorthWest
  opposite DueEast = DueWest
  opposite NorthEast = SouthWest

  cw90 DueNorth = DueEast
  cw90 NorthEast = SouthEast
  cw90 DueEast = DueSouth
  cw90 SouthEast = SouthWest
  cw90 DueSouth = DueWest
  cw90 SouthWest = NorthWest
  cw90 DueWest = DueNorth
  cw90 NorthWest = NorthEast

  ccw90 DueNorth = DueWest
  ccw90 NorthWest = SouthWest
  ccw90 DueWest = DueSouth
  ccw90 SouthWest = SouthEast
  ccw90 DueSouth = DueEast
  ccw90 SouthEast = NorthEast
  ccw90 DueEast = DueNorth
  ccw90 NorthEast = NorthWest

cw45 :: Dir8 -> Dir8
cw45 DueNorth = NorthEast
cw45 NorthEast = DueEast
cw45 DueEast = SouthEast
cw45 SouthEast = DueSouth
cw45 DueSouth = SouthWest
cw45 SouthWest = DueWest
cw45 DueWest = NorthWest
cw45 NorthWest = DueNorth

ccw45 :: Dir8 -> Dir8
ccw45 DueNorth = NorthWest
ccw45 NorthWest = DueWest
ccw45 DueWest = SouthWest
ccw45 SouthWest = DueSouth
ccw45 DueSouth = SouthEast
ccw45 SouthEast = DueEast
ccw45 DueEast = NorthEast
ccw45 NorthEast = DueNorth

fromDir4 :: Dir4 -> Dir8
fromDir4 North = DueNorth
fromDir4 West = DueWest
fromDir4 South = DueSouth
fromDir4 East = DueEast

dir8s :: [Dir8]
dir8s =
  [ DueNorth
  , NorthWest
  , DueWest
  , SouthWest
  , DueSouth
  , SouthEast
  , DueEast
  , NorthEast]

cardinals8 :: [Dir8]
cardinals8 = map fromDir4 dir4s

ordinals8 :: [Dir8]
ordinals8 = [NorthWest, SouthWest, SouthEast, NorthEast]
