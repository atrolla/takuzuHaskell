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

-- solve

solve :: Grid -> Grid
solve = head . filter valid . combine . choices

-- choices

type Choices = [Digit]

choices :: Grid -> Matrix Choices
choices = undefined

-- combine

combine :: Matrix Choices -> [Grid]
combine = undefined

-- valid

valid :: Grid -> Bool
valid g = undefined