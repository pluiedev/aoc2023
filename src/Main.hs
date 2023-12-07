module Main where

import Test.Hspec
import qualified Day1
import qualified Day2
import qualified Day3
import qualified Day4
import qualified Day5
import qualified Day6
import qualified Day7

main :: IO ()
main = hspec $ do
  Day1.test
  Day2.test
  Day3.test
  Day4.test
  Day5.test
  Day6.test
  Day7.test
