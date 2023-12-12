module Day11 where

import Data.List (findIndices, transpose)
import Data.Maybe (catMaybes)
import Data.Set (Set)
import Data.Set qualified as Set
import Test.Hspec (Spec, describe)
import Text.Megaparsec
import Text.Megaparsec.Char
import Util.Aoc2023
import Util.Aoc2023.Vec2 (Vec2 (..), taxicabDist)

test :: Spec
test = describe "Day 11" $ do
  let solnA = soln 2 -- 2 because one empty row/col is now 2
      solnB = soln 1000000

  example 11 1 1 $ parseShouldBe 374 solnA
  input 11 1 $ parseShouldBe 9418609 solnA
  example 11 2 1 $ \s -> do
    parseShouldBe 1030 (soln 10) s
    parseShouldBe 8410 (soln 100) s
  input 11 2 $ parseShouldBe 593821230983 solnB

soln :: Int -> Parser Int
soln ex = sum . dists . pointSet ex <$> parseGrid

parseGrid :: Parser [[Bool]]
parseGrid = some $ some (True <$ char '#' <|> False <$ char '.') <* newline

pointSet
  :: Int -- factor of expansion - how many rows/cols should an empty row/col count as
  -> [[Bool]] -- raw grid - True is # (galaxy), False is . (empty space)
  -> Set (Vec2 Int) -- set of all positions of galaxies, adjusted for empty space
pointSet ex pp = Set.fromList $ rows pp
  where
    allFalseIndices = Set.fromList . findIndices (all not)
    emptyRows = allFalseIndices pp
    emptyCols = allFalseIndices $ transpose pp

    offset s x = x + (ex - 1) * Set.size (fst $ Set.split x s)

    row y' = catMaybes . zipWith
      (\x b -> if b then Just (Vec2 (offset emptyCols x) y') else Nothing)
      [0..]
    rows = concat . zipWith (row . offset emptyRows) [0..]

dists :: Set (Vec2 Int) -> [Int]
dists ps = [p1 `taxicabDist` p2
  | p1 <- Set.toList ps
  -- we do something quite clever here —- instead of going through all points,
  -- we can just pair with all points that are "greater" than the current
  -- point, as any lesser points would just treat the current point as one
  -- of the "greater" nodes. as such, we effectively halve our output space,
  -- and we can avoid needing to sort through cases where p1 = p2 since p2 > p1.
  -- sadly still O(n^2) though
  , p2 <- Set.toList $ snd $ Set.split p1 ps]
