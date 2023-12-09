module Day9 where

import Test.Hspec (Spec, describe, it)
import Text.Megaparsec
import Text.Megaparsec.Char
import Util.Aoc2023

test :: Spec
test = describe "Day 9" $ do
  example 9 1 1 $ parseShouldBe 114 solnA
  input 9 1 $ parseShouldBe 1916822650 solnA
  example 9 2 1 $ parseShouldBe 2 solnB
  input 9 2 $ parseShouldBe 966 solnB

solnA :: Parser Int
solnA = parseInput $ sum . map last

-- I just ***RANDOMLY*** guessed a fold based on the end values,
-- and holy shit it just f'ing works
-- I love Haskell
solnB :: Parser Int
solnB = parseInput $ foldr (-) 0 . map head

parseInput :: ([[Int]] -> Int) -> Parser Int
parseInput f =
  sum . map (f . seqs) . filter (not . null)
  <$> integer `sepBy` hspace `sepEndBy` newline

seqs :: [Int] -> [[Int]]
seqs xs
  | null xs || all (== 0) xs = [xs]
  | otherwise = xs : seqs (zipAdjacent (flip (-)) xs)

-- basically makes a sliding window with size 2
zipAdjacent :: (a -> a -> b) -> [a] -> [b]
zipAdjacent f = zipWith f <*> tail

