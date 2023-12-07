module Day5 where

import Data.Either (partitionEithers)
import Data.Maybe
import Test.Hspec
import Text.Megaparsec.Char
import Text.Megaparsec
import Util.Aoc2023

test :: SpecWith ()
test = describe "Day 5" $ do
  it "passes the part 1 example" $
    readFile "src/Day5/example" >>= parseShouldBe 35 solnA
  it "passes the part 1 solution" $
    readFile "src/Day5/input" >>= parseShouldBe 199602917 solnA
  it "passes the part 2 example" $
    -- readFile "src/Day5/example" >>= parseTest solnB
    pendingWith "performance sucks"
  it "passes the part 2 solution" $
    -- readFile "src/Day5/input" >>= parseShouldBe 46 solnB
    pendingWith "performance sucks"

solnA :: Parser Int
solnA = do
  s <- seeds
  fs <- foldl (flip (.)) id . map applyOne <$> many parseMap
  return $ minimum $ map fs s

-- solnB :: Parser Int
-- solnB = do
--   s <- seeds
--   fs <- map applyRange <$> many parseMap
--
--   return $ map (\[s,e] ->
--       foldl (\r f -> f r) [(s, s+e)] fs
--     ) $ chunksOf 2 s

seeds :: Parser [Int]
seeds = string "seeds: " *> (integer `sepBy` hspace) <* newline <* newline

parseMap :: Parser [(Int, Int, Int)]
parseMap = skipManyTill anySingle (string ":\n") *> (mapStep `sepEndBy` newline)

mapStep :: Parser (Int, Int, Int)
mapStep = (,,)
  <$> integer <* hspace
  <*> integer <* hspace
  <*> integer <* hspace

applyOne :: [(Int, Int, Int)] -> Int -> Int
applyOne ls i =
  fromMaybe i $
  listToMaybe $
  mapMaybe (\(dst, src, w) ->
    let n = i - src in
    if n >= 0 && n < w then Just (dst + n) else Nothing
  )
  ls

-- Inspired by https://github.com/jonathanpaulson/AdventOfCode/blob/49a2901c16d9941657226893b9bda49f66d19633/2023/5.py

applyRange :: [(Int, Int, Int)] -> [(Int, Int)] -> [(Int, Int)]
applyRange ls r =
  uncurry (++) $
  foldl (\(a, r') (dst, src, w) ->
    let
      sort (s, e)
        | snd pre > fst pre = Right pre
        | snd inter > fst inter
        = Left (fst inter - src + dst, snd inter - src + dst)
        | otherwise = Right post
        where
          pre = (s, min e src)
          inter = (max s src, min (src + w) e)
          post = (max (src + w) s, e)
      (na, nr) = partitionEithers $ map sort r'
    in (a ++ na, nr)
  ) ([], r) ls

