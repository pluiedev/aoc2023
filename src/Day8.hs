module Day8 where

import Data.Map.Strict (Map, (!))
import qualified Data.Map.Strict as Map
import Test.Hspec (Spec, describe)
import Text.Megaparsec
import Text.Megaparsec.Char
import Util.Aoc2023

test :: Spec
test = describe "Day 8" $ do
  example 8 1 1 $ parseShouldBe 2 solnA
  example 8 1 2 $ parseShouldBe 6 solnA
  input 8 1 $ parseShouldBe 22411 solnA
  example 8 2 2 $ parseShouldBe 6 solnB
  input 8 2 $ const unimplemented

data Direction = DLeft | DRight deriving (Eq, Show)

walkA :: [Direction] -> Map String Node -> Int -> String -> Int
walkA _ _ i "ZZZ" = i
walkA (d:ds) map i cur =
  walkA ds map (i + 1) $ case d of
    DLeft -> l
    DRight -> r
  where (l, r) = map ! cur

walkB :: [Direction] -> Map String Node -> Int -> [String] -> Int
walkB (d:ds) m i curs =
  if all (\c -> last c == 'Z') curs then
    i
  else
    walkB ds m (i + 1) $ map (case d of
      DLeft -> fst . (!) m
      DRight -> snd . (!) m) curs

type Node = (String, String)

solnA :: Parser Int
solnA = do
  (dirs, map) <- parseInput
  return $ walkA (cycle dirs) map 0 "AAA"

solnB :: Parser Int
solnB = do
  (dirs, map) <- parseInput
  return $ walkB (cycle dirs) map 0 $
    filter (\c -> last c == 'A') (Map.keys map)

parseInput :: Parser ([Direction], Map String Node)
parseInput = (,)
  <$> some dir <* newline <* newline
  <*> (Map.fromList <$> node `sepEndBy` newline)
  where
    dir = DLeft <$ char 'L' <|> DRight <$ char 'R'
    label = some (digitChar <|> upperChar)
    node = (,)
      <$> label <* hspace <* char '=' <* hspace
      <*> between (char '(') (char ')') (
        (,) <$> label <* string ", " <*> label
      )

