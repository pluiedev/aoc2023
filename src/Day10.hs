module Day10 where

import Control.Arrow (second)
import Data.Array.IArray
import Data.Function (on)
import Data.List (groupBy, nub, sortOn)
import Data.Maybe (fromJust, mapMaybe)
import Data.Set (Set)
import Data.Set qualified as Set
import Test.Hspec (Spec, describe)
import Text.Megaparsec hiding (Pos)
import Text.Megaparsec.Char
import Util.Aoc2023
import Util.Aoc2023.Array
import Util.Aoc2023.Dir (Dir4 (..), opposite, dir4s)
import Util.Aoc2023.Vec2

test :: Spec
test = describe "Day 10" $ do
  example 10 1 1 $ parseShouldBe 8 solnA
  input 10 1 $ parseShouldBe 6864 solnA
  example 10 2 1 $ const unimplemented
  input 10 2 $ const unimplemented

printGrid :: Map -> IO ()
printGrid =
  mapM_
    (\a -> mapM_ printCell a >> putStr "\n")
    . groupBy ((==) `on` (y . fst))
    . sortOn (y . fst)
    . assocs
  where
    printCell :: (Pos, (Tile, Int)) -> IO ()
    printCell (_, (_, s)) =
      let r = if s == 0 then "." else show s
       in putStr (r ++ replicate (4 - length r) ' ')

type Pos = Vec2 Int

data Tile
  = Ground
  | NorthSouth
  | EastWest
  | NorthEast
  | NorthWest
  | SouthEast
  | SouthWest
  | Start
  deriving (Eq, Ord)

instance Show Tile where
  show Ground = "."
  show NorthSouth = "|"
  show EastWest = "-"
  show NorthEast = "L"
  show NorthWest = "J"
  show SouthEast = "F"
  show SouthWest = "7"
  show Start = "S"

connected :: Dir4 -> Tile -> Bool
connected North = flip elem [NorthSouth, NorthEast, NorthWest, Start]
connected West = flip elem [EastWest, NorthWest, SouthWest, Start]
connected South = flip elem [NorthSouth, SouthEast, SouthWest, Start]
connected East = flip elem [EastWest, NorthEast, SouthEast, Start]

solnA :: Parser Int
solnA = uncurry run <$> parseMap

parseMap :: Parser (Pos, Map)
parseMap = do
  tiles <- some ((,0 :: Int) <$> parseTile) `sepEndBy` newline
  let m = array2 tiles
      cur = fromJust $ findArrIndex ((==) Start . fst) m
  return (cur, m // [(cur, (fst $ m ! cur, 1))])

type Map = Array Pos (Tile, Int)

run :: Pos -> Map -> Int
run ps m = run' (Set.singleton ps, 0, m)
  where
    run' a = case update a of
      (True, (_, mx, _)) -> mx - 1
      (False, a') -> run' a'

update :: (Set Pos, Int, Map) -> (Bool, (Set Pos, Int, Map))
update (ps, mx, m) = (length totalDeltas < 2, (Set.fromList psf, mxf, mf))
  where
    mf = m // nub totalDeltas
    (psf, totalDeltas, mxf) = foldl f ([], [], mx) $ map (step m) $ Set.elems ps
    f (psa, ua, maxa) delta =
      ( psa <> map fst delta,
        ua <> delta,
        foldl (\n x -> max n (snd $ snd x)) maxa delta
      )

step :: Map -> Pos -> [(Pos, (Tile, Int))]
step m s = mapMaybe mapper $ neighbors s
  where
    nHere = snd (m ! s) + 1

    mapper (d, p) = m !? p >>= criterion
      where
        criterion (t, n)
          | n == 0 && connected (opposite d) t = Just (p, (t, nHere))
          | otherwise = Nothing

neighbors :: Pos -> [(Dir4, Pos)]
neighbors p = map (\d -> (d, moveTo d p)) dir4s

parseTile :: Parser Tile
parseTile =
  choice
    [ Ground <$ char '.',
      NorthSouth <$ char '|',
      EastWest <$ char '-',
      NorthEast <$ char 'L',
      NorthWest <$ char 'J',
      SouthEast <$ char 'F',
      SouthWest <$ char '7',
      Start <$ char 'S'
    ]
