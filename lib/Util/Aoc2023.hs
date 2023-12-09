module Util.Aoc2023 where

import Data.Void (Void)
import Test.Hspec
import Text.Megaparsec
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void String

integer :: Parser Int
integer = L.signed (return ()) L.decimal

uInteger :: Parser Int
uInteger = L.lexeme (return ()) L.decimal

surroundedBy :: Applicative m => m a -> m sur -> m a
surroundedBy i o = o *> i <* o

readInput :: String -> Int -> IO String
readInput name day = readFile ("src/Day" ++ show day ++ "/" ++ name)

input :: Int -> Int -> (String -> Expectation) -> Spec
input day part f = it ("passes the part " ++ show part ++ " solution") $ readInput "input" day >>= f

example :: Int -> Int -> Int -> (String -> Expectation) -> Spec
example day part n f = it ("passes the part " ++ show part ++ " example " ++ show n) $
  readInput ("example" ++ show n) day >>= f

parseShouldBe :: (Show a, Stream s, Ord e, Eq a) => a -> Parsec e s a -> s -> Expectation
parseShouldBe v p = (`shouldBe` Just v) . parseMaybe p

parseShouldNotBe :: (Show a, Stream s, Ord e, Eq a) => a -> Parsec e s a -> s -> Expectation
parseShouldNotBe v p = (`shouldNotBe` Just v) . parseMaybe p

shouldSatisfyAll :: (HasCallStack, Show a) => a -> [a -> Bool] -> Expectation
shouldSatisfyAll v = mapM_ (v `shouldSatisfy`)

unimplemented :: Expectation
unimplemented = pendingWith "unimplemented"

-- Blackbird (B_1) combinator
(.:) :: (c -> d) -> (a -> b -> c) -> a -> b -> d
(.:) = (.) . (.)
