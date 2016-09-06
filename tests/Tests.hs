module Main where

import Test.Hspec
import Test.QuickCheck
import Takuzu

main :: IO()
main = hspec $ do
  describe "deduce does" $ do
    it "replace a blank with the only possible symbol using triplicate rule" $ do
      deduce "00." `shouldBe` "001"
      deduce "0.0" `shouldBe` "010"
      deduce ".00" `shouldBe` "100"
      deduce ".000" `shouldBe` "1000"
      deduce "0.00" `shouldBe` "0100"
      deduce "00.00" `shouldBe` "00100"
      deduce "01100" `shouldBe` "01100"
      deduce "011.0" `shouldBe` "01100"
  describe "fill does" $ do
    it "replaces remaining blanks using balance rule" $ do
      fill "0." `shouldBe` "01"
      fill "1." `shouldBe` "10"
      fill "1.1." `shouldBe` "1010"
      fill "1.1..." `shouldBe` "1.1..."

  describe "loop does" $ do
    it "apply a function till the result is the same as input" $ do
      let function x | x == 3 = x
                     | otherwise = x + 1
          func2 x | head x == 'b' = x
                  | otherwise = tail x
      loop func2 "qsmldkfjbqsdf" `shouldBe` "bqsdf"
