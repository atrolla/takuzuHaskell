module Main where

import Test.Hspec
import Test.QuickCheck
import Takuzu

main :: IO()
main = hspec $ do
  describe "choices does" $ do
    it "try all combination of possible symbol for blank entries" $ do
      choices [[]] `shouldBe` [[]]
      