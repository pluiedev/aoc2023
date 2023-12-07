module Util.Aoc2023 where

import Data.Void (Void)
import Test.Hspec
import Text.Megaparsec
import Text.Megaparsec.Char

type Parser = Parsec Void String

integer :: Parser Int
integer = read <$> some digitChar

surroundedBy :: Applicative m => m a -> m sur -> m a
surroundedBy i o = o *> i <* o

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
