module Day3 where

import Test.Hspec (Spec, describe)
import Util.Aoc2023
import Text.Megaparsec.Char (digitChar, char)
import Text.Megaparsec (parseTest, noneOf, choice)
import Util.Aoc2023.Array (array2, (!?))
import Data.Array (assocs, Array)
import Util.Aoc2023.Dir (dir8s)
import Util.Aoc2023.Vec2 (Vec2)
import Util.Aoc2023.Vec2 qualified as Vec2
import Data.Maybe (mapMaybe)
import Control.Monad ((>=>))
import Data.Containers.ListUtils (nubOrd)

test :: Spec
test = describe "Day 3" $ do
  let
    solnA = soln mapperA
    solnB = soln mapperB
  example 3 1 1 $ parseTest solnA
  input 3 1 $ parseTest solnA
  example 3 2 1 $ parseTest solnB
  input 3 2 $ parseTest solnB

data Cell = Digit Char | Symbol | Gear | Empty deriving (Show, Eq)
type Condensed = Either Cell Int

soln :: (Array (Vec2 Int) Condensed -> (Vec2 Int, Condensed) -> Int) -> Parser Int
soln f = do
  grid <- array2 . map condense <$> parseGrid parseCell
  return $ sum . map (f grid) $ assocs grid

parseCell :: Parser Cell
parseCell = choice
  [ Digit  <$> digitChar
  , Empty  <$  char '.'
  , Gear   <$  char '*'
  , Symbol <$  noneOf "\n"]

uniqueNeighbors :: Array (Vec2 Int) Condensed -> Vec2 Int -> [Int]
uniqueNeighbors grid p = nubOrd $ mapMaybe (((!?) grid >=> rightToMaybe) . (`Vec2.moveTo8` p)) dir8s

mapperA :: Array (Vec2 Int) Condensed -> (Vec2 Int, Condensed) -> Int
mapperA grid (p, Left _) = sum $ uniqueNeighbors grid p
mapperA _ _ = 0

mapperB :: Array (Vec2 Int) Condensed -> (Vec2 Int, Condensed) -> Int
mapperB grid (p, Left Gear) = case filter (/= 0) $ uniqueNeighbors grid p of
  [a, b] -> a * b
  _ -> 0
mapperB _ _ = 0

condense :: [Cell] -> [Condensed]
condense [] = []
condense xs@(Digit _:_) = number 0 [] xs
  where
    number n ds (Digit d:as) = number (n + 1) (ds ++ [d]) as
    number n ds as = replicate n (Right $ read ds) ++ condense as
condense (Empty:xs) = Right 0 : condense xs
condense (c:xs) = Left c : condense xs

rightToMaybe :: Either a b -> Maybe b
rightToMaybe (Left _) = Nothing
rightToMaybe (Right b) = Just b
