module Day8 where

import Control.Monad (liftM2)
import Data.List (elemIndex, findIndex)
import Data.Maybe (fromMaybe)
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
  example 8 2 3 $ parseShouldBe 6 solnB
  input 8 2 $ parseShouldBe 11188774513823 solnB

walk
  :: (String -> Bool)   -- end criterion
  -> Map String Node    -- map of nodes
  -> [Direction]        -- list of directions, should be infinite
  -> String             -- starting node
  -> Int
walk f m = walk' 0
  where
    pickDir DLeft = left
    pickDir DRight = right
    walk' i (d:ds) cur
      | f cur = i
      | otherwise = walk' (i + 1) ds $ pickDir d $ m ! cur

data Node = Node { left :: String, right :: String }
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
parseInput = (,)
  <$> (cycle <$> some dir <* newline <* newline)
  <*> (Map.fromList <$> node `sepEndBy` newline)
  where
    dir = DLeft <$ char 'L' <|> DRight <$ char 'R'
    label = some $ digitChar <|> upperChar
    node = (,)
      <$> label <* hspace <* char '=' <* hspace
      <*> between (char '(') (char ')') (
        Node <$> label <* string ", " <*> label
      )

