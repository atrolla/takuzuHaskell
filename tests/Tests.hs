module Main where

import Test.Hspec
import Test.QuickCheck
import Takuzu

main :: IO()
main = hspec $ do
  describe "choices does" $ do
    it "try all combination of possible symbol for blank entries" $ do
      choices [[]] `shouldBe` [[]]
      choices [['1']] `shouldBe` [[['1']]]
      choices [['.']] `shouldBe` [[['0','1']]]
      choices [['.','1','1','0']] `shouldBe` [[['0','1'],['1'],['1'],['0']]]
      
  --describe "combine does"
    --it "combine choices to grids"
      
      
  describe "valid does" $ do
    describe "check that a Takuzu" $ do
      it "is filled" $ do
        isFilled [] `shouldBe` False
        isFilled ['1'] `shouldBe` True
        isFilled ['.'] `shouldBe` False
        isFilled ['.','1'] `shouldBe` False
        isFilled ['.','0'] `shouldBe` False
        
      it "contains an equal number of '1' and '0'" $ do
        isBalanced [] `shouldBe` False
        isBalanced ['0','1'] `shouldBe` True
        isBalanced ['0','1','0','1'] `shouldBe` True
        isBalanced ['0','1','1'] `shouldBe` False
        
      it "has not 3 same and consecutives number" $ do
        noTriplicate [] `shouldBe` True
        noTriplicate ['1','1','1'] `shouldBe` False
        noTriplicate ['1','1','1','0'] `shouldBe` False
        noTriplicate ['0','1','1','1','0'] `shouldBe` False
        noTriplicate ['0','1','1','0','0'] `shouldBe` True
  
      it "has each row unique" $ do
        isUnique [[]] `shouldBe` True
        isUnique [['0'],['1']] `shouldBe` True
    
    it "OK" $ do      
      valid [[]]  `shouldBe` False
      valid [['0','1','1','0'],['1','0','0','1'],['0','0','1','1'],['1','1','0','0']]  `shouldBe` True
      valid [['0','0','1','1'],['1','1','0','0'],['0','0','1','1'],['1','1','0','0']]  `shouldBe` False
      valid [['.','1','1','0'],['1','0','0','1'],['0','0','1','1'],['1','1','0','0']]  `shouldBe` False
      
  describe "solve does" $ do
    it "use all previous functions to find a solution for a Takuzu" $ do
      solve [['.','1','1','0'],['1','0','0','1'],['0','0','1','1'],['1','1','0','0']]  `shouldBe` [['0','1','1','0'],['1','0','0','1'],['0','0','1','1'],['1','1','0','0']]
      
  describe "deduce does" $ do
    describe "fill does" $ do
      it "replace choices with the only valid possible symbol" $ do
        fill [['0','1'],['0'],['0']] `shouldBe` [['1'],['0'],['0']]
        fill [['0'],['0','1'],['0']] `shouldBe` [['0'],['1'],['0']]
        fill [['0'],['0','1'],['0'],['0']] `shouldBe` [['0'],['1'],['0'],['0']]
        fill [['0'],['0'],['0','1'],['0'],['0']] `shouldBe` [['0'],['0'],['1'],['0'],['0']]
  
    it "OK" $ do
      deduce [[['0','1'],['0'],['0']]] `shouldBe` [[['1'],['0'],['0']]]
      
  describe "solve2 does" $ do
    it "use all previous functions to find a solution for a Takuzu" $ do
      solve2 [['.','1','1','0'],['1','0','0','1'],['0','0','1','1'],['1','1','0','0']]  `shouldBe` [['0','1','1','0'],['1','0','0','1'],['0','0','1','1'],['1','1','0','0']]
      
  describe "checkvalid" $ do
    it "is like filter valid" $ property $
      \g -> checkvalid g == filter valid g