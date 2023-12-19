module Day19 where

import Test.Hspec (Spec, describe)
import Util.Aoc2023
import Text.Megaparsec (between, choice, some, sepBy, sepEndBy, parseTest, MonadParsec (try))
import Text.Megaparsec.Char (char, lowerChar, string, newline)
import Data.Map (Map, (!))
import Data.Map qualified as Map
import Data.Maybe (mapMaybe, catMaybes)

test :: Spec
test = describe "Day 19" $ do
  example 19 1 1 $ parseShouldBe 19114 solnA
  input 19 1 $ parseShouldBe 401674 solnA
  example 19 2 1 $ parseTest solnB
  input 19 2 $ parseTest solnB

solnA :: Parser Int
solnA = sum . map rating . uncurry run <$> parseInputA

-- solnA :: Parser Int

-- {a<2006:qkq,m>2090:A,rfg}
-- {a>1716:R,A}
--
-- a:
-- 0..2005 -> QKQ
--
--
parseInputA :: Parser (Map.Map String Workflow, [Part])
parseInputA = (,)
  <$> parseWorkflows
  <*> parsePart `sepEndBy` newline

parseWorkflows :: Parser (Map.Map String Workflow)
parseWorkflows = Map.fromList <$> parseWorkflow `sepEndBy` newline <* newline

-- bifurcate :: Map String Workflow -> Field -> (Int, Int) -> Int
-- bifurcate m f = bifurcate' (rules first) (fallback first)
--   where
--     first = m ! "in"

aa n m Accepted f = [Just f]
aa n m Rejected _ = [Nothing]
aa n m (Continue c) f =
  let Workflow { rules, fallback } = m ! c
  in bifurcate' c m rules fallback f

solnB = do
  workflows <- parseWorkflows
  let
    Workflow {rules, fallback} = workflows ! "in"

  return (sum $ map scoreFlow $ catMaybes $ bifurcate' "in" workflows rules fallback (Flow (1, 4000) (1, 4000) (1, 4000) (1, 4000)))

bifurcate' n m [] fb flow = aa n m fb flow
bifurcate' n m ((Rule field ty num outcome):rs) fb flow =
  case ty of
    IfLesser -> let
        (p, q) = divert field (num - 1) flow
      in aa n m outcome p ++ bifurcate' n m rs fb q
    IfGreater -> let
        (p, q) = divert field num flow
      in aa n m outcome q ++ bifurcate' n m rs fb p

data Flow = Flow
  (Int, Int)
  (Int, Int)
  (Int, Int)
  (Int, Int)
  deriving Show

splitRange :: Int -> (Int, Int) -> ((Int, Int), (Int, Int))
splitRange n (s, e)
  | n < s = ((s, s), (s, e))
  | n > e = ((s, e), (e, e))
  | otherwise = ((s, n), (n + 1, e))

divert :: Field -> Int -> Flow -> (Flow, Flow)
divert X n (Flow x m a s) = both (\v -> Flow v m a s) $ splitRange n x
divert M n (Flow x m a s) = both (\v -> Flow x v a s) $ splitRange n m
divert A n (Flow x m a s) = both (\v -> Flow x m v s) $ splitRange n a
divert S n (Flow x m a s) = both (Flow x m a) $ splitRange n s

scoreFlow (Flow x m a s) = diff x * diff m * diff a * diff s
  where diff (start, end) = end - start + 1

both :: (a -> b) -> (a, a) -> (b, b)
both f (a, b) = (f a, f b)

run :: Map String Workflow -> [Part] -> [Part]
run m = mapMaybe (run' (m ! "in"))
  where
    run' :: Workflow -> Part -> Maybe Part
    run' Workflow { rules, fallback } p = case findFirstOr fallback (`runRule` p) rules of
      Continue c -> run' (m ! c) p
      Accepted -> Just p
      Rejected -> Nothing

findFirstOr :: b -> (a -> Maybe b) -> [a] -> b
findFirstOr a _ [] = a
findFirstOr a f (x:xs) = case f x of
  Just j -> j
  Nothing -> findFirstOr a f xs


data Workflow = Workflow { rules :: [Rule], fallback :: Outcome } deriving Show

parseWorkflow :: Parser (String, Workflow)
parseWorkflow = (,)
  <$> some lowerChar
  <*> (Workflow
    <$> (char '{' *> some (try parseRule <* char ','))
    <*> parseOutcome <* char '}'
  )

data Part = Part { x :: Int, m :: Int, a :: Int, s :: Int } deriving Show

parsePart :: Parser Part
parsePart = Part
  <$> (string "{x=" *> integer)
  <*> (string ",m=" *> integer)
  <*> (string ",a=" *> integer)
  <*> (string ",s=" *> integer)
  <* char '}'

fieldOfPart :: Field -> Part -> Int
fieldOfPart X = x
fieldOfPart M = m
fieldOfPart A = a
fieldOfPart S = s

rating :: Part -> Int
rating (Part x m a s) = x + m + a + s

data Field = X | M | A | S deriving (Show, Eq)
parseField :: Parser Field
parseField = choice
  [ X <$ char 'x'
  , M <$ char 'm'
  , A <$ char 'a'
  , S <$ char 's' ]

data Rule = Rule
  { field :: Field
  , ty :: RuleType
  , cmp :: Int
  , outcome :: Outcome }
  deriving Show
data RuleType = IfGreater | IfLesser deriving (Show, Eq)

runRule :: Rule -> Part -> Maybe Outcome
runRule (Rule f t n o) p
  | t == IfGreater && fieldOfPart f p > n = Just o
  | t == IfLesser  && fieldOfPart f p < n = Just o
  | otherwise = Nothing

parseRule :: Parser Rule
parseRule = Rule
  <$> parseField
  <*> choice
    [ IfGreater <$ char '>'
    , IfLesser <$ char '<' ]
  <*> integer
  <*> (char ':' *> parseOutcome)

data Outcome = Accepted | Rejected | Continue String deriving Show

parseOutcome :: Parser Outcome
parseOutcome = choice
  [ Accepted <$  char 'A'
  , Rejected <$  char 'R'
  , Continue <$> some lowerChar ]
