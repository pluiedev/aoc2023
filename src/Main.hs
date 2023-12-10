module Main where

import Day1 qualified
import Day2 qualified
import Day3 qualified
import Day4 qualified
import Day5 qualified
import Day6 qualified
import Day7 qualified
import Day8 qualified
import Day9 qualified
import Test.Hspec

main :: IO ()
main = hspec $ do
  Day1.test
  Day2.test
  Day3.test
  Day4.test
  Day5.test
  Day6.test
  Day7.test
  Day8.test
  Day9.test
