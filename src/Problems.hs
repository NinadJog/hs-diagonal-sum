module Problems (
  diagonalDifference, sumDiag, createMatrix
) where

import Data.Array

{-
Problem 1. Calculate the absolute difference between the sum of the main
diagonal and the opposite (secondary) diagonal. 

For example, the absolute diagonal difference of the following matrix 
  [[1, 2, 15],
   [4, 5, 6],
   [0, 8, 1]]
is |(1 + 5 + 1) - (15 + 5 + 0)| = |7 - 20| = 13.
-}
diagonalDifference :: Array (Int, Int) Int -> Int
diagonalDifference matrix =
  abs (a - b)
    where
      a = sumDiag True matrix   -- Sum of main diagonal
      b = sumDiag False matrix  -- Sum of opposite diagonal

----------------------------------------------------
{-
Calculates the sum of the main diagonal if the Boolean is True,
otherwise calculates the sum of the opposite (secondary) diagonal.

Implementation: Takes rows in increasing order of indices.
Column numbers are in increasing order for the main diagonal
and decreasing order for the secondary diagonal.

For example if the row indices of a 3x3 matrix are 1, 2, 3
and the column indices are 7, 8, 9 then the indices of the 
opposite diagonal are (1,9), (2,8) and (3,7).
-}
sumDiag :: Bool -> Array (Int, Int) Int -> Int
sumDiag isMain matrix =
  sum [matrix ! ind | ind <- indexes] -- ind is a tuple such as (1,7)
    where
      indexes   = zip [r1..r2] colRange
      colRange  = if' isMain [c1..c2] [c2, c2-1..c1]
      ((r1, c1), (r2, c2)) = bounds matrix

----------------------------------------------------
-- A helper function for if-then-else
if' :: Bool -> a -> a -> a
if' True x _  = x
if' False _ y = y

----------------------------------------------------
{-
Creates a 2-d array from a list of list of Int.
Needed only while testing.
-}
createMatrix :: (Int, Int) -> (Int, Int) -> [[Int]] -> Array (Int, Int) Int
createMatrix (r1, c1) (r2, c2) xss = 
  listArray ((r1, c1), (r2, c2)) (concat xss)

----------------------------------------------------
-- Incidental Functions
{- I came up with the following functions along the way towards
creating the above functions. These functions aren't needed
for the problem's solution but I'm including them here anyway.
-}

