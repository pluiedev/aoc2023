module Util.Aoc2023 where

import Data.Void (Void)
import Test.Hspec
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer qualified as L

type Parser = Parsec Void String

surroundedBy :: (Applicative m) => m a -> m sur -> m a
surroundedBy i o = o *> i <* o

readInput :: String -> Int -> IO String
readInput name day = readFile ("src/Day" ++ show day ++ "/" ++ name)

input :: Int -> Int -> (String -> Expectation) -> Spec
input day part f = it ("passes the part " ++ show part ++ " solution") $ readInput "input" day >>= f

example :: Int -> Int -> Int -> (String -> Expectation) -> Spec
example day part n f =
  it ("passes the part " ++ show part ++ " example " ++ show n) $
    readInput ("example" ++ show n) day >>= f

parseShouldBe :: (Show a, VisualStream s, TraversableStream s, ShowErrorComponent e, Ord e, Eq a) => a -> Parsec e s a -> s -> Expectation
parseShouldBe v p s = case parse p "" s of
  Left e -> expectationFailure (errorBundlePretty e)
  Right r -> r `shouldBe` v


parseShouldNotBe :: (Show a, VisualStream s, TraversableStream s, ShowErrorComponent e, Ord e, Eq a) => a -> Parsec e s a -> s -> Expectation
parseShouldNotBe v p s = case parse p "" s of
  Left e -> expectationFailure (errorBundlePretty e)
  Right r -> r `shouldNotBe` v

shouldSatisfyAll :: (HasCallStack, Show a) => a -> [a -> Bool] -> Expectation
shouldSatisfyAll v = mapM_ (v `shouldSatisfy`)

unimplemented :: Expectation
unimplemented = pendingWith "unimplemented"

-- Shared parsers

parseGrid :: Parser a -> Parser [[a]]
parseGrid p = some p `sepEndBy` newline

boolCell :: Parser Bool
boolCell = True <$ char '#' <|> False <$ char '.'

printBoolGrid :: [[Bool]] -> IO ()
printBoolGrid bs = do
  mapM_ (putStrLn . map (\b -> if b then '#' else '.')) bs
  putChar '\n'

integer :: Parser Int
integer = L.signed (return ()) L.decimal

uInteger :: Parser Int
uInteger = L.lexeme (return ()) L.decimal

-- Blackbird (B_1) combinator
infixr 9 .:

(.:) :: (c -> d) -> (a -> b -> c) -> a -> b -> d
(.:) = (.) . (.)
