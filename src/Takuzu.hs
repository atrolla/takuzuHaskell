module Takuzu where

import Data.List
-- types

type Matrix a = [Row a]
type Digit = Char
type Row a = [a]

type Grid = Matrix Digit

blank :: Digit
blank = '.'

isBlank :: Digit -> Bool
isBlank = (==blank)

symbols :: [Digit]
symbols = ['0','1']

-- solve

solve :: Grid -> Grid
solve = loop (map rowRules)
  where rowRules = deduce . fill

--

deduce :: Row Digit -> Row Digit
deduce (x:y:z:zs)
  | y == z && not (isBlank y) && isBlank x = inv y : deduce (y:z:zs)
  | x == z && not (isBlank x) && isBlank y = x : deduce ((inv x):z:zs)
  | x == y && not (isBlank x) && isBlank z = x : deduce (y:(inv x):zs)
  | otherwise                              = x : deduce (y:z:zs)
    where inv '1' = '0'
          inv _   = '1'
deduce l = l

-- fill

fill :: Row Digit -> Row Digit
fill ls
  | full '1'  = map (replace '0') ls
  | full '0'  = map (replace '1') ls
  | otherwise = ls
  where full x = halfLength == occurence x
        occurence x = (length $ filter (==x) ls)
        halfLength = (length ls) `div` 2
        replace x y | y == '.'  = x
                    | otherwise = y

-- loop

loop :: (Eq a) => (a -> a) -> a -> a
loop f x
  | y == x = x
  | otherwise = loop f y
  where y = f x

-- valid

valid :: Grid -> Bool
valid _ = undefined
