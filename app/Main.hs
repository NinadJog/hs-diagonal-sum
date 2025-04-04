module Main (main) where

import Problems
import Utilities

main :: IO ()
main = do

  -- Problem 1. Absolute Diagonal Difference of a Square Matrix
  let
    matrix      = createMatrix (1, 7) (3, 9) [[1, 2, 15],
                                              [4, 5, 6],
                                              [0, 8, 1]]
    -- row indices are [1..3], col indices are [7..9]
                                            
    diagSum     = sumDiag True matrix
    oppSumDiag  = sumDiag False matrix
    diagDiff    = diagonalDifference matrix

  putStrLn $ mconcat ["Diag Sum         = ", show diagSum]    -- 7
  putStrLn $ mconcat ["Opp Diag Sum     = ", show oppSumDiag] -- 20
  putStrLn $ mconcat ["Diag Difference  = ", show diagDiff]   -- 13

  -- Problem 2. Proportion of positives, negatives and zeros in an array
  let ratios = plusMinusZero [1, -1, 0, -1, 2]
  printList ratios
