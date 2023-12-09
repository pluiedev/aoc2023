module Day4 where

import Data.Function (on)
import qualified Data.Set as Set
import Test.Hspec (Spec, describe)
import Text.Megaparsec
import Text.Megaparsec.Char
import Util.Aoc2023
import Control.Monad (liftM2)

test :: Spec
test = describe "Day 4" $ do
  example 4 1 1 $ parseShouldBe 13 solnA
  input 4 1 $ parseShouldBe 18519 solnA
  example 4 2 1 $ parseShouldBe 30 solnB
  input 4 2 $ parseShouldBe 11787590 solnB

solnA :: Parser Int
solnA = sum . map ((^) 2 . pred) . filter (/= 0) <$> many card

solnB :: Parser Int
solnB = liftM2 go id (flip replicate (1::Int) . length) <$> many card
  where
    go (c:cs) (n:ns) = n + go cs (map (+ n) (take c ns) ++ drop c ns)
    go _ _ = 0

card :: Parser Int
card = (Set.size .: (Set.intersection `on` Set.fromList))
  <$> (between (string "Card" *> hspace) (char ':') integer *> nums)
  <*> nums

nums :: Parser [Int]
nums = manyTill (surroundedBy integer hspace) $ oneOf "|\n"
