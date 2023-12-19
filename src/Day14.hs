module Day14 where

import Test.Hspec (Spec, describe)
import Text.Megaparsec
import Text.Megaparsec.Char
import Util.Aoc2023
import Data.Set (Set)
import Data.Set qualified as Set
import Util.Aoc2023.Vec2 (Vec2(..))
import Util.Aoc2023.Vec2 qualified as Vec2
import Data.Maybe (mapMaybe, listToMaybe, fromMaybe, fromJust)
import Data.Array (assocs, Ix (inRange), bounds, Array)
import Util.Aoc2023.Array (array2)
import Util.Aoc2023.Dir (Dir4 (..), Dir(opposite))

test :: Spec
test = describe "Day 14" $ do
  -- example 14 1 1 $ parseShouldBe 136 solnA
  example 14 1 1 $ parseTest solnA
  -- input 14 1 $ parseShouldBe 109345 solnA
  -- example 14 2 1 $ \s -> do
    -- print $ foldlTillCycle (\x n -> (x + n) `mod` 5) 0 (repeat 1)
  -- example 14 2 1 $ parseTest solnB
  -- input 14 2 $ parseTest solnB

data Space = Rounded | Cube | Empty deriving (Show, Eq, Ord)


spaceCell :: Parser Space
spaceCell = choice [
  Rounded <$ char 'O',
  Cube <$ char '#',
  Empty <$ char '.']

solnA = do
  grid <- array2 <$> parseGrid spaceCell
  let
    (balls, cubes) = lysis grid
    boundz = bounds grid


    (pos, balls') = fromJust $ Set.minView balls
    ball = obstacle boundz pos North (cubes <> balls')
    cubes' = Set.insert ball cubes
    (pos', balls'') = fromJust $ Set.minView balls'
    ball' = obstacle boundz pos North (cubes' <> balls'')


  return (ball, balls'')

  -- return $ score boundz $ roll boundz cubes balls North

solnB = do
  grid <- array2 <$> parseGrid spaceCell
  let
    (balls, cubes) = lysis grid
    boundz = bounds grid

    (uniq, cycl) = aCycle (rollCycle boundz cubes) balls
    predict n
      | n < length uniq = reverse uniq !! n
      | otherwise = reverse cycl !! ((n - 1 - length uniq) `mod` length cycl)

  -- return (score (predict
  return (length uniq, length cycl, score boundz (predict
    1_000_000_000
    -- 1
    ))

score :: (Vec2 Int, Vec2 Int) -> Set (Vec2 Int) -> Int
score (_,maxB) = Set.foldr ((+) . (maxY -) . Vec2.y) 0
  where maxY = 1 + Vec2.y maxB

aCycle :: (Eq b) => (b -> b) -> b -> ([b], [b])
aCycle = go []
  where
    go bs f acc = case biggestCycle of
      Just cy -> cy
      Nothing -> go (acc' : bs) f acc'
      where
        acc' = f acc
        biggestCycle = listToMaybe $ mapMaybe split $ reverse [1..length bs `div` 2]
        split n
          | first == second = Just (sfx, first)
          | otherwise = Nothing
          where
            -- [XXXX= pfx =XXXX|xxx= first =xxx|..= first =..]
            (dups, sfx) = splitAt (n * 2) bs
            (first, second) = splitAt n dups

-- shifter :: ([a], [a], [a]) -> ([a], [a], [a])
-- shifter (pfx, first, second) = let

lysis :: Array (Vec2 Int) Space -> (Set (Vec2 Int), Set (Vec2 Int))
lysis bss = (poses Rounded, poses Cube)
  where
    poses b = Set.fromList $ map fst $ filter ((== b) . snd) $ assocs bss

rollCycle :: (Vec2 Int, Vec2 Int) -> Set (Vec2 Int) -> Set (Vec2 Int) -> Set (Vec2 Int)
rollCycle bs cubes balls = foldl (roll bs cubes) balls [North, West, South, East]

roll :: (Vec2 Int, Vec2 Int) -> Set (Vec2 Int) -> Set (Vec2 Int) -> Dir4 -> Set (Vec2 Int)
roll bs cubes balls dir = Set.difference cubes $ run cubes balls
  -- | balls' == balls = balls'
  -- | otherwise = roll bs cubes balls' dir
  where
    run os ns = case Set.minView ns of
      Just (ball, ns') -> let o = obstacle bs ball dir os
        in run (Set.insert o os) ns'
      Nothing -> os



    -- balls' = Set.map go balls
    -- go ball
    --   | not (inRange bs ball') || ball' `Set.member` cubes || ball' `Set.member` balls = ball
    --   | otherwise = ball'
    --  :petpetvi: where ball' = Vec2.moveTo dir ball

obstacle :: (Vec2 Int, Vec2 Int) -> Vec2 Int -> Dir4 -> Set (Vec2 Int) -> Vec2 Int
obstacle (Vec2 xMin yMin, Vec2 xMax yMax) (Vec2 bx by) dir = case dir of
  North -> obstacle' Set.lookupMax (Vec2 bx yMin) $ \(Vec2 x y) -> x == bx && y < by
  South -> obstacle' Set.lookupMin (Vec2 bx yMax) $ \(Vec2 x y) -> x == bx && y > by
  West  -> obstacle' Set.lookupMax (Vec2 xMin by) $ \(Vec2 x y) -> x < bx && y == by
  East  -> obstacle' Set.lookupMin (Vec2 xMax by) $ \(Vec2 x y) -> x > bx && y == by
  where
    obstacle' look borderValue = maybe borderValue (Vec2.moveTo (opposite dir)) .: look .: Set.filter

-- Honorable mention of my initial solution, which doesn't really
-- simulate the rocks rolling but uses some nice grouping magic
-- to achieve the same effect. Doesn't work with part 2 though
-- solnA :: Parser Int
-- solnA =
--   sum
--   . map (
--     sum
--     . zipWith (\i n -> if n == Rounded then i else 0) [1..]
--     . reverse
--     . concatMap sort
--     . groupBy ((&&) `on` (/= Cube))
--   )
--   . transpose
--   <$> parseGrid spaceCell
