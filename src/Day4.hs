module Day4 where

import Data.Function (on)
import qualified Data.Set as Set
import Test.Hspec
import Text.Megaparsec
import Text.Megaparsec.Char
import Util.Aoc2023
import Control.Monad (liftM2)

test :: SpecWith ()
test = describe "Day 4" $ do
  it "passes the part 1 example" $
    readFile "src/Day4/example" >>= parseShouldBe 13 solnA
  it "passes the part 1 solution" $
    readFile "src/Day4/input" >>= parseShouldBe 18519 solnA
  it "passes the part 2 example" $
    readFile "src/Day4/example" >>= parseShouldBe 30 solnB
  it "passes the part 2 solution" $
    readFile "src/Day4/input" >>= parseShouldBe 11787590 solnB

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
