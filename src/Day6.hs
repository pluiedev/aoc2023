module Day6 where

import Data.Function (on)
import Test.Hspec
import Text.Megaparsec
import Text.Megaparsec.Char
import Util.Aoc2023

test :: SpecWith ()
test = describe "Day 6" $ do
  it "passes the part 1 example" $
    readFile "src/Day6/example" >>= parseShouldBe 288 solnA
  it "passes the part 1 solution" $
    readFile "src/Day6/input" >>= parseShouldBe 128700 solnA
  it "passes the part 2 example" $
    readFile "src/Day6/example" >>= parseShouldBe 71503 solnB
  it "passes the part 2 solution" $ do
    readFile "src/Day6/input" >>= parseShouldBe 39594072 solnB

solnA :: Parser Int
solnA = product .: zipWith options <$> line integer <*> line integer

solnB :: Parser Int
solnB = (options `on` read) <$> line digitChar <*> line digitChar

line :: (Token s ~ Char, MonadParsec e s m) => m a -> m [a]
line p = between
  (many letterChar *> char ':' *> hspace) newline $
  many (p <* hspace)

options :: Int -> Int -> Int
-- options t d = length [() | h <- [0..t], h * (t - h) > d]
options t d = ((-) `on` ceiling . flip (/) 2) (t' + c) (t' - c)
  where -- faster math™ magic
    t' = fromIntegral t :: Double
    d' = fromIntegral d
    c = sqrt (t' * t' - 4.0 * (d' + 0.01))
