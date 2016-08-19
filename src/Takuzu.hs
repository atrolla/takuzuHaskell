module Takuzu where

import Data.List
-- types

type Matrix a = [[a]]
type Digit = Char

type Grid = Matrix Digit

blank :: Digit
blank = '.'

isBlank :: Digit -> Bool
isBlank = (==blank)

symbols :: [Digit]
symbols = ['0','1']

--

solve :: Grid -> Grid
solve = head . filter valid . combine . choices

solve2 :: Grid -> Grid
solve2 = head . checkvalid . combine2 . choices

-- deduce

deduce :: Matrix Choices -> Matrix Choices
deduce = map fill

fill :: [Choices] -> [Choices]
fill (x:y:z:xs)
  | x == y && (not (isSingleton z)) && isSingleton x = fill (x:x:(inv x):xs)
  | x == z && (not (isSingleton y)) && isSingleton x = fill (x:(inv x):x:xs)
  | y == z && (not (isSingleton x)) && isSingleton y = fill ((inv z):z:z:xs)
  | otherwise = x:(fill (y:z:xs))
fill l = l

isSingleton :: Choices -> Bool
isSingleton [_] = True
isSingleton _ = False

inv :: Choices -> Choices
inv ['0'] = ['1']
inv _ = ['0']

-- choices

type Choices = [Digit]

choices :: Grid -> Matrix Choices
choices = map (map possibilities)
  where possibilities x | isBlank x = symbols
                        | otherwise = [x]


-- combine

combine :: Matrix Choices -> [Grid]
combine = sequence . map sequence

combine2 :: Matrix Choices -> [Grid]
combine2 = sequence . map (optim . sequence)
  where optim = filter isBalanced . filter noTriplicate
  
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
valid g = all (isFilled) g && check g && check (transpose g)

check x = isUnique x && all (isBalanced) x && all (noTriplicate) x

checkvalid = filter (check . transpose) . filter check . filter (all isFilled)