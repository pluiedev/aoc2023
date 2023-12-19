module Day18 where

import Test.Hspec (Spec, describe)
import Util.Aoc2023
import Text.Megaparsec (choice, between, sepEndBy, parseTest, count)
import Util.Aoc2023.Dir (Dir4(..))
import Text.Megaparsec.Char (char, hspace, string, hexDigitChar, newline)
import Util.Aoc2023.Vec2 (Vec2(..))
import Util.Aoc2023.Vec2 qualified as Vec2
import Data.Set (Set)
import qualified Data.Set as Set
import Control.Monad.Trans.State (State, get, gets, put, execState, evalState)
import Data.Maybe (catMaybes)
import Data.Array (Ix(inRange))

test :: Spec
test = describe "Day 18" $ do
  example 18 1 1 $ parseTest (flip evalState initS . (>> fillCount) . mapM_ dig <$> parseInput)
  -- input 18 1 $ parseTest (flip evalState initS . (>> fillCount) . mapM_ dig <$> parseInput)
  input 18 1 $ const unimplemented
  example 18 2 1 $ const unimplemented
  input 18 2 $ const unimplemented

data DiggerState = DiggerState
  { points :: Set (Vec2 Int)
  , pos :: Vec2 Int
  , bounds :: (Vec2 Int, Vec2 Int)}
  deriving (Show)

initS = DiggerState { points = Set.empty, pos = Vec2 0 0, bounds = (Vec2 0 0, Vec2 0 0)}

-- fillCount :: State DiggerState Int
fillCount = do
  DiggerState { points, bounds = (Vec2 xMin yMin, Vec2 xMax yMax) } <- get
  return (cleanse $ Set.toList points)
  -- return (cleanse (filter (inRange (yMin + 1, yMax - 1) . Vec2.y) $ Set.toList points))

-- cleanse bs = zipWith Vec2.taxicabDist bs (tail bs)
cleanse (b:bs) = evalState (mapM cleanseOne bs) (b, False)

-- cleanseOne :: Vec2 Int -> State (Vec2 Int, Bool) Int
cleanseOne p'@(Vec2 x' y') = do
  (p@(Vec2 x y), taking) <- get
  let taking' = if x' - x > 1 then not taking else y == y' && taking
  put (p', taking')
  return (if x' - x > 1 && taking' then (x' - x, p, p') else (1, p, p'))


dig :: (Dir4, Int, Int) -> State DiggerState ()
dig (d, n, _) = do
  DiggerState {points, pos, bounds = (pMin, pMax)} <- get
  let
    newPts = scanl (\p _ -> d `Vec2.moveTo` p) pos [1..n]
    newPtsSet = Set.fromList newPts
  put $ DiggerState
    { points = points <> newPtsSet
    , pos = last newPts
    , bounds = (pMin `min` Set.findMin newPtsSet, pMax `max` Set.findMax newPtsSet) }

parseInput = ((,,) <$> parseDir <* hspace <*> integer <* hspace <*> parseHexColor) `sepEndBy` newline

parseDir :: Parser Dir4
parseDir = choice
  [ East <$ char 'R'
  , South <$ char 'D'
  , West <$ char 'L'
  , North <$ char 'U' ]

parseHexColor :: Parser Int
parseHexColor = read . ("0x" ++ ) <$> between (string "(#") (char ')') (count 6 hexDigitChar)
