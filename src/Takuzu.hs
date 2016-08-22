module Takuzu where

import Data.List
-- types

type Matrix a = [Row a]
type Row a = [a]
type Digit = Char

type Grid = Matrix Digit

blank :: Digit
blank = '.'

isBlank :: Digit -> Bool
isBlank = (==blank)

symbols :: [Digit]
symbols = ['0','1']

-- solve

solve :: Grid -> Grid
solve = head . filter valid . combine . choices

solve2 :: Grid -> Grid
solve2 g = head $ intersect solveByRow solveByCol
    where solveByRow = (combine . choices) g
          solveByCol = (map transpose . combine . choices . transpose) g

-- choices

type Choices = Row Digit

choices :: Grid -> Matrix Choices
choices = map (rowChoices)

rowChoices :: Choices -> Row Choices
rowChoices r = nub $ map (convert r) allPossibilities
    where allPossibilities = permutations $ possibilities r 

convert :: Choices -> Choices -> Choices
convert r p 
    | r == [] = []
    | rhIsBlank = ph : convert rt pt
    | not rhIsBlank = rh : convert rt p
    | otherwise = [] -- not necessary
        where rhIsBlank = isBlank rh
              (rh:rt) = r
              (ph:pt) = p

possibilities :: Choices -> Choices
possibilities r = concatMap (\s -> replicate (remaining s) s) symbols
    where   remaining x = halfLength - occurences x
            occurences x = length $ filter (==x) r
            halfLength = (length r) `div` 2

-- combine

combine :: Matrix Choices -> [Grid]
combine = sequence

-- valid
isFilled :: [Digit] -> Bool
isFilled [] = False
isFilled l = blank `notElem` l

isBalanced :: [Digit] -> Bool
isBalanced []  = False
isBalanced l  = numberOf '1' l == numberOf '0' l
  where numberOf c xs = length $ filter (==c) xs

noTriplicate :: [Digit] -> Bool
noTriplicate (x:y:z:xs) = not triplicate && noTriplicate l
  where l = y:z:xs
        triplicate = (x == y && y == z)
noTriplicate _ = True

isUnique :: Grid -> Bool
isUnique [] = True
isUnique ( x : xs ) = all ( /= x ) xs && isUnique xs

valid :: Grid -> Bool
valid = check [all (isFilled), row, row . transpose]
    where row = check [isUnique, all (isBalanced), all (noTriplicate)]
          check l x = foldl (&&) True $ map (\f -> f x) l
          