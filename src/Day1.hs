module Day1 where

import Control.Monad (liftM2)
import Data.Char (ord)
import Data.List.Split (splitOn)
import Test.Hspec
import Text.Megaparsec
import Text.Megaparsec.Char
import Util.Aoc2023
import Safe

test :: SpecWith ()
test = describe "Day 1" $ do
  it "passes the part 1 example" $
    readFile "src/Day1/example" >>= parseShouldBe 142 (soln digit)
  it "passes the part 1 solution" $
    readFile "src/Day1/input" >>= parseShouldBe 55172 (soln digit)
  it "passes the part 2 example" $
    readFile "src/Day1/example2" >>= parseShouldBe 281 (soln digitB)
  it "passes the part 2 solution" $
    readFile "src/Day1/input" >>= parseShouldNotBe 54953 (soln digitB)

soln :: Parser Int -> Parser Int
soln d =
   sum .
   map (liftM2 (+) ((*) 10 . headDef 0) (lastDef 0)) .
   splitOn [0] <$> many (skipManyTill lowerChar $ 0 <$ newline <|> d)

digit :: Parser Int
digit = flip (-) 0x30 . ord <$> digitChar

digitB :: Parser Int
digitB = choice
  [digit,
  1 <$ string "one",
  2 <$ string "two",
  3 <$ string "three",
  4 <$ string "four",
  5 <$ string "five",
  6 <$ string "six",
  7 <$ string "seven",
  8 <$ string "eight",
  9 <$ string "nine"]
