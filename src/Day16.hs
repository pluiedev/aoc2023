module Day16 where

import Test.Hspec (Spec, describe)
import Util.Aoc2023
import Util.Aoc2023.Vec2 (Vec2(..))
import Util.Aoc2023.Dir (Dir4(..))
import Util.Aoc2023.Vec2 qualified as Vec2
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Array (Ix(inRange), Array, (!), bounds)
import Text.Megaparsec (choice, parseTest)
import Text.Megaparsec.Char (char)
import Util.Aoc2023.Array (array2)

test :: Spec
test = describe "Day 16" $ do
  example 16 1 1 $ parseTest soln
  input 16 1 $ parseTest soln
  example 16 2 1 $ parseTest solnB
  input 16 2 $ parseTest solnB

soln = do
  grid <- array2 <$> parseGrid parseCell
  return $ Set.size $ Set.map fst $ step grid Set.empty [(Vec2 0 0, East)]

solnB = do
  grid <- array2 <$> parseGrid parseCell
  let
    (_, Vec2 maxX maxY) = bounds grid
    starts = [[(Vec2 x 0, South), (Vec2 x maxY, North)] | x <- [0..maxX]]
      ++ [[(Vec2 0 y, East), (Vec2 maxX y, West)] | y <- [0..maxY]]
  return $ foldl max 0 . map (Set.size . Set.map fst . step grid Set.empty . pure) $ concat starts

step :: Array (Vec2 Int) Cell -> Set (Vec2 Int, Dir4) -> [(Vec2 Int, Dir4)] -> Set (Vec2 Int, Dir4)
step _ set [] = set
step arr set ps =
  let ps' = filter (\p -> inRange (bounds arr) (fst p) && p `Set.notMember` set) ps
  in step arr (set <> Set.fromList ps') [
    (Vec2.moveTo d' p, d') | (p, d) <- ps', d' <- bounce d (arr ! p) ]
  -- in [(Vec2.moveTo d' p, d') | (p, d) <- ps', d' <- bounce d $ arr ! p ]

data Cell
  = Empty       -- .
  | RMirror     -- /
  | LMirror     -- \
  | VSplitter   -- |
  | HSplitter   -- -
  deriving (Show, Eq)

parseCell :: Parser Cell
parseCell = choice
  [ Empty     <$ char '.'
  , RMirror   <$ char '/'
  , LMirror   <$ char '\\'
  , VSplitter <$ char '|'
  , HSplitter <$ char '-']

-- Calculates the position of where the beam should go next
bounce :: Dir4 -> Cell -> [Dir4]
bounce d RMirror = case d of
  North -> [East]
  East -> [North]
  South -> [West]
  West -> [South]
bounce d LMirror = case d of
  North -> [West]
  West -> [North]
  South -> [East]
  East -> [South]
bounce East VSplitter = [North, South]
bounce West VSplitter = [North, South]
bounce North HSplitter = [West, East]
bounce South HSplitter = [West, East]
bounce d _ = [d]
