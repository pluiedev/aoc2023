module Day2 where

import Test.Hspec (Spec, describe)
import Text.Megaparsec
import Text.Megaparsec.Char
import Util.Aoc2023

test :: Spec
test = describe "Day 2" $ do
  example 2 1 1 $ parseShouldBe 8 (soln mapA)
  input 2 1 $ parseShouldBe 1867 (soln mapA)
  example 2 2 1 $ parseShouldBe 2286 (soln mapB)
  input 2 2 $ parseShouldBe 84538 (soln mapB)

data Game = Game Int Cubes deriving (Show, Eq)

data Cubes = Cubes Int Int Int deriving (Show, Eq)

instance Semigroup Cubes where
  (Cubes r1 g1 b1) <> (Cubes r2 g2 b2) =
    Cubes
      (r1 `max` r2)
      (g1 `max` g2)
      (b1 `max` b2)

instance Monoid Cubes where
  mempty = Cubes 0 0 0

soln :: (Game -> Int) -> Parser Int
soln f = sum . map f <$> gameA `sepEndBy` newline

mapA :: Game -> Int
mapA (Game i (Cubes r g b))
  | r > 12 || g > 13 || b > 14 = 0
  | otherwise = i

mapB :: Game -> Int
mapB (Game _ (Cubes r g b)) = r * g * b

gameA :: Parser Game
gameA =
  Game
    <$> between (string "Game ") (string ": ") integer
    <*> (mconcat . mconcat <$> cube `sepBy` string ", " `sepBy` string "; ")

cube :: Parser Cubes
cube =
  flip id
    <$> integer
    <* space
    <*> choice
      [ (\n -> Cubes n 0 0) <$ string "red",
        (\n -> Cubes 0 n 0) <$ string "green",
        Cubes 0 0 <$ string "blue"
      ]
