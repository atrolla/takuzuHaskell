module Main where

import Test.Hspec
import Test.QuickCheck
import Takuzu
import Data.List

main :: IO()
main = hspec $ do
  describe "choices does" $ do
    describe "possibilities" $ do
        it "returns all remaining symbols for blank entries of a row" $ do
            possibilities ['.','.'] `shouldBe` "01"
            possibilities ['0','.'] `shouldBe` "1"
            possibilities ['0','.','0','.','.','.'] `shouldBe` "0111"
    describe "convert" $ do
        it "replace blank entries with a possibility" $ do
            convert ['0'] [] `shouldBe` ['0']
            convert ['.'] ['1'] `shouldBe` ['1']
            convert ['0','.'] ['1'] `shouldBe` ['0','1']
            convert ['.','0'] ['1'] `shouldBe` ['1','0']
    describe "rowChoices" $ do
        it "try all combination of possibilities for a row with blank entries" $ do
            rowChoices ['0'] `shouldBe` [['0']]
            rowChoices ['0','.'] `shouldBe` [['0','1']]
            rowChoices ['1','.'] `shouldBe` [['1','0']]
            sort (rowChoices ['1','.','0','.']) `shouldBe` sort ([['1','1','0','0'],['1','0','0','1']])
            sort (rowChoices ['.','.','0','.']) `shouldBe` sort (["0101","1001","1100"])
    it "try all combination of possibilities on a grid" $ do
        choices [['0']] `shouldBe` [[['0']]]
        choices [['1','.','0','.']] `shouldBe` [sort ([['1','1','0','0'],['1','0','0','1']])]
        
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