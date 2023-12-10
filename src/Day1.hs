module Day1 where

import Control.Monad (liftM2)
import Data.Char (ord)
import Data.List.Split (splitOn)
import Safe
import Test.Hspec (Spec, describe)
import Text.Megaparsec
import Text.Megaparsec.Char
import Util.Aoc2023

test :: Spec
test = describe "Day 1" $ do
  example 1 1 1 $ parseShouldBe 142 (soln digit)
  input 1 1 $ parseShouldBe 55172 (soln digit)
  example 1 2 2 $ parseShouldBe 281 (soln wordOrDigit)
  input 1 2 $ parseShouldNotBe 54953 (soln wordOrDigit)

soln :: Parser Int -> Parser Int
soln d =
  sum
    . map (liftM2 (+) ((*) 10 . headDef 0) (lastDef 0))
    . splitOn [0]
    <$> many (skipManyTill lowerChar $ 0 <$ newline <|> d)

digit :: Parser Int
digit = flip (-) 0x30 . ord <$> digitChar

wordOrDigit :: Parser Int
wordOrDigit =
  choice
    [ digit,
      1 <$ string "one",
      2 <$ string "two",
      3 <$ string "three",
      4 <$ string "four",
      5 <$ string "five",
      6 <$ string "six",
      7 <$ string "seven",
      8 <$ string "eight",
      9 <$ string "nine"
    ]
