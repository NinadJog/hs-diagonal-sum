module Problems (
  diagonalDifference, sumDiag, createMatrix,
  plusMinusZero, plusMinusZero2
) where

import Data.Array
import Utilities

-------------------------------------------------------------------------------
-- Problem 1. Diagonal Difference
-------------------------------------------------------------------------------
{-
Calculate the absolute difference between the sum of the main
diagonal and the opposite (secondary) diagonal of a 2-D matrix

For example, the absolute diagonal difference of the following matrix 
  [[1, 2, 15],
   [4, 5, 6],
   [0, 8, 1]]
is |(1 + 5 + 1) - (15 + 5 + 0)| = |7 - 20| = 13.
-}
diagonalDifference :: Array (Int, Int) Int -> Int
diagonalDifference matrix = abs $ a - b
  where
    a = sumDiag True matrix   -- Sum of main diagonal
    b = sumDiag False matrix  -- Sum of opposite diagonal

-------------------------------------------------------------------------------
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
sumDiag isMain matrix = total
  where
    total     = sum [matrix ! ind | ind <- indexes] -- ind is a tuple such as (1,7)
    indexes   = zip [r1..r2] colRange
    colRange  = if' isMain [c1..c2] [c2, c2-1..c1]
    ((r1, c1), (r2, c2)) = bounds matrix

----------------------------------------------------
{-
Helper function to create a matrix (2-d array) from a list of list of Int.
Needed only while testing.
-}
createMatrix :: (Int, Int) -> (Int, Int) -> [[Int]] -> Array (Int, Int) Int
createMatrix (r1, c1) (r2, c2) xss = 
  listArray ((r1, c1), (r2, c2)) (concat xss)

-------------------------------------------------------------------------------
-- Problem 2. Proportions of Positive, Negative, Zero
-------------------------------------------------------------------------------
{-
Find the proportions of integers that are positive, negative and
zero from a given list. Return them in a list in that order.
Implementation using list comprehensions.
-}
plusMinusZero :: [Int] -> [Double]
plusMinusZero [] = []
plusMinusZero xs = (ratios len) <$> [pos, neg, zero]
  where
    len   = length xs
    pos   = sum [1 | x <- xs, x > 0]
    neg   = sum [1 | x <- xs, x < 0]
    zero  = sum [1 | x <- xs, x == 0]

    ratios :: Int -> Int -> Double
    ratios n y  = (fromIntegral y) / (fromIntegral n)

----------------------------------------------------
-- Implementation using recursion
plusMinusZero2 :: [Int] -> [Double]
plusMinusZero2 [] = []
plusMinusZero2 ys = plusMinusZero2' ys 0 0 0
  where
    plusMinusZero2' [] pos neg zero = (ratios len) <$> [pos, neg, zero]
    plusMinusZero2' (x:xs) pos neg zero
      | x > 0     = plusMinusZero2' xs (pos + 1) neg zero
      | x < 0     = plusMinusZero2' xs pos (neg + 1) zero
      | otherwise = plusMinusZero2' xs pos neg (zero + 1)

    len = length ys
    ratios :: Int -> Int -> Double
    ratios n y  = (fromIntegral y) / (fromIntegral n)




