module Day15 where

import Test.Hspec (Spec, describe)
import Util.Aoc2023
import Text.Megaparsec.Char (char, lowerChar)
import Text.Megaparsec (noneOf, sepBy, some, (<|>))
import Data.Char (ord)
import Data.Map qualified as Map
import Data.Maybe (fromMaybe)

test :: Spec
test = describe "Day 15" $ do
  example 15 1 1 $ parseShouldBe 1320 solnA
  input 15 1 $ parseShouldBe 508552 solnA
  example 15 2 1 $ parseShouldBe 145 solnB
  input 15 2 $ parseShouldBe 265462 solnB

solnA :: Parser Int
solnA = sum . map hash <$> parseInput (some (noneOf ",\n"))

solnB :: Parser Int
solnB =
  sum .
  map (uncurry powerAll) .
  Map.toList .
  foldl (flip stepInit) Map.empty
  <$> parseInput lens
  where
    lens = Lens <$> some lowerChar <*>
      (Remove <$ char '-' <|>
      Append <$> (char '=' *> integer))

    powerAll box = sum . zipWith (power box) [0..]
    stepInit l = Map.alter (removeOrAppend l) $ hash $ label l

removeOrAppend :: Lens -> Maybe [Lens] -> Maybe [Lens]
removeOrAppend l ls = Just (first ++ h ++ drop 1 second)
  where
    (first, second) = span ((/= label l) . label) (fromMaybe [] ls)
    h = case action l of
      Remove -> []
      Append _ -> [l]

power :: Int -> Int -> Lens -> Int
power box slot l = (box + 1) * (slot + 1) * fLength (action l)

hash :: String -> Int
hash = foldl (\a n -> 17 * (a + ord n) `mod` 256) 0

parseInput :: Parser a -> Parser [a]
parseInput = (`sepBy` char ',')

data Lens = Lens { label :: String, action :: Action } deriving (Show, Eq, Ord)
data Action = Remove | Append Int deriving (Show, Ord)
instance Eq Action where
  Remove == Remove = True
  Append _ == Append _ = True
  _ == _ = False

fLength :: Action -> Int
fLength Remove = 0
fLength (Append i) = i
