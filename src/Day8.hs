module Day8 where

import Data.Map.Strict (Map, (!))
import Data.Map.Strict qualified as Map
import Test.Hspec (Spec, describe)
import Text.Megaparsec
import Text.Megaparsec.Char
import Util.Aoc2023

test :: Spec
test = describe "Day 8" $ do
  example 8 1 1 $ parseShouldBe 2 solnA
  example 8 1 2 $ parseShouldBe 6 solnA
  input 8 1 $ parseShouldBe 22411 solnA
  example 8 2 3 $ parseShouldBe 6 solnB
  input 8 2 $ parseShouldBe 11188774513823 solnB

walk ::
  (String -> Bool) -> -- end criterion
  Map String Node -> -- map of nodes
  [Direction] -> -- list of directions, should be infinite
  String -> -- starting node
  Int
walk f m = walk' 0
  where
    pickDir DLeft = left
    pickDir DRight = right
    walk' i [] _ = i
    walk' i (d : ds) cur
      | f cur = i
      | otherwise = walk' (i + 1) ds $ pickDir d $ m ! cur

data Node = Node {left :: String, right :: String}

data Direction = DLeft | DRight deriving (Eq, Show)

solnA :: Parser Int
solnA = do
  (ds, m) <- parseInput
  return $ walk (== "ZZZ") m ds "AAA"

solnB :: Parser Int
solnB = do
  (ds, m) <- parseInput
  return $
    foldl lcm 1 $
      map (walk ((==) 'Z' . last) m ds) $
        filter ((==) 'A' . last) $
          Map.keys m

parseInput :: Parser ([Direction], Map String Node)
parseInput =
  (,)
    <$> (cycle <$> some dir <* newline <* newline)
    <*> (Map.fromList <$> ((,) <$> nodeKey <*> node) `sepEndBy` newline)
  where
    dir = DLeft <$ char 'L' <|> DRight <$ char 'R'
    lbl = some $ digitChar <|> upperChar
    nodeKey = lbl <* char '=' `surroundedBy` hspace
    node =
      between
        (char '(')
        (char ')')
        $ Node <$> lbl <* string ", " <*> lbl
