module Day7 where

import Data.Char (ord)
import Data.List (group, partition, sort, sortOn)
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Ord
import Test.Hspec (Spec, describe, it, shouldBe, shouldSatisfy)
import Text.Megaparsec
import Text.Megaparsec.Char
import Util.Aoc2023

test :: Spec
test = describe "Day 7" $ do
  let checkKinds c = mapM_ (\(s, k) -> handType (c s) `shouldBe` k)
      compareHands c = mapM_ (\(a, b) -> c a `shouldSatisfy` (< c b))

  it "passes part 1 sanity checks" $ do
    let c = fromList CardA
    checkKinds
      c
      [ ("AAAAA", FiveOfAKind),
        ("AA8AA", FourOfAKind),
        ("23332", FullHouse),
        ("TTT98", ThreeOfAKind),
        ("23432", TwoPair),
        ("A23A4", OnePair),
        ("23456", HighCard)
      ]
    compareHands
      c
      [ ("32T3K", "KTJJT"),
        ("KTJJT", "KK677"),
        ("KK677", "T55J5"),
        ("T55J5", "QQQJA")
      ]

  example 7 1 1 $ parseShouldBe 6440 (soln CardA)
  input 7 1 $ parseShouldBe 252052080 (soln CardA)

  it "passes part 2 sanity checks" $ do
    let c = fromList CardB
    checkKinds
      c
      [ ("JJJJJ", FiveOfAKind),
        ("555J5", FiveOfAKind),
        ("KJJJT", FourOfAKind),
        ("QJJQ2", FourOfAKind),
        ("QQ9JA", ThreeOfAKind)
      ]
    compareHands
      c
      [ ("JKKK2", "QQQQ2"),
        ("T55J5", "QQQJA"),
        ("QQQJA", "KTJJT")
      ]

  example 7 2 1 $ parseShouldBe 5905 (soln CardB)
  input 7 2 $ parseShouldBe 252898370 (soln CardB)

soln :: (Card c) => (Char -> c) -> Parser Int
soln c =
  sum . zipWith (\rank (_, bet) -> rank * bet) [1 ..] . sort
    <$> sepEndBy
      ( (,)
          <$> parseCards
          <* spaceChar
          <*> integer
      )
      newline
  where
    parseCards = Hand . map c <$> some (oneOf "23456789TJQKA")

-- Card

class (Eq c, Ord c) => Card c where
  lengthOfGroups :: [c] -> [Int]

lengthOfGroups' :: (Card c) => [c] -> [Int]
lengthOfGroups' = sortOn Down . map length . group . sort

newtype CardA = CardA Char deriving (Show, Eq)

instance Ord CardA where
  compare = comparing $ \(CardA c) ->
    case c of
      'A' -> 14
      'K' -> 13
      'Q' -> 12
      'J' -> 11
      'T' -> 10
      other -> ord other - 0x30

instance Card CardA where
  lengthOfGroups = lengthOfGroups'

newtype CardB = CardB Char deriving (Show, Eq)

instance Ord CardB where
  compare = comparing $ \(CardB c) -> case c of
    'A' -> 14
    'K' -> 13
    'Q' -> 12
    'J' -> 1
    'T' -> 10
    other -> ord other - 0x30

instance Card CardB where
  lengthOfGroups cs = length jokers + fromMaybe 0 (listToMaybe lengths) : drop 1 lengths
    where
      (jokers, nonjokers) = partition (== CardB 'J') cs
      lengths = lengthOfGroups' nonjokers

-- Hand

newtype Hand c = Hand {cards :: [c]} deriving (Show, Eq)

instance (Card c) => Ord (Hand c) where
  compare = comparing handType <> comparing cards

fromList :: (Card c) => (Char -> c) -> [Char] -> Hand c
fromList c s =
  if length s == 5
    then Hand $ map c s
    else error "Hand not 5 cards big"

-- Hand type

data HandType
  = HighCard
  | OnePair
  | TwoPair
  | ThreeOfAKind
  | FullHouse
  | FourOfAKind
  | FiveOfAKind
  deriving (Show, Eq, Ord)

handType :: (Card c) => Hand c -> HandType
handType (Hand h) =
  let h' = lengthOfGroups h -- [5]
   in case head h' of
        5 -> FiveOfAKind
        4 -> FourOfAKind
        3 -> if h' !! 1 == 2 then FullHouse else ThreeOfAKind
        2 -> if h' !! 1 == 2 then TwoPair else OnePair
        1 -> HighCard
        _ -> error "somehow we have more than 5 cards per hand"
