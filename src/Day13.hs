module Day13 where

import Data.Bits (xor)
import Data.Function (on)
import Data.List (transpose)
import Test.Hspec (Spec, describe)
import Text.Megaparsec
import Text.Megaparsec.Char
import Util.Aoc2023

test :: Spec
test = describe "Day 13" $ do
  let solnA = soln (==)
      solnB = soln diffByOne
  example 13 1 1 $ parseShouldBe 405 solnA
  input 13 1 $ parseShouldBe 31956 solnA
  example 13 2 1 $ parseShouldBe 400 solnB
  input 13 2 $ parseShouldBe 37617 solnB

soln :: ([[Bool]] -> [[Bool]] -> Bool) -> Parser Int
soln f = sum . map (comps f) <$> parseGrid boolCell `sepBy` newline

comp :: (Eq a) => ([[a]] -> [[a]] -> b) -> [[a]] -> Int -> b
comp f rows col = f (drop (length l - len) l) (reverse (take len r))
  where
    (l, r) = splitAt col rows
    len = (min `on` length) l r

diffByOne :: [[Bool]] -> [[Bool]] -> Bool
diffByOne = (== 1) .: sum .: zipWith (length .: filter id .: zipWith xor)

comps :: (Eq a) => ([[a]] -> [[a]] -> Bool) -> [[a]] -> Int
comps f rows = 100 * summed rows + summed (transpose rows)
  where
    summed xs = sum $ filter (comp f xs) [1 .. length xs - 1]
