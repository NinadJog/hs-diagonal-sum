module Main (main) where

import Problems

main :: IO ()
main = do

  -- Problem 1. Absolute Diagonal Difference of a Square Matrix
  let
    matrix      = createMatrix (1, 7) (3, 9) [[1, 2, 15],
                                              [4, 5, 6],
                                              [0, 8, 1]]
    diagSum     = sumDiag True matrix
    oppSumDiag  = sumDiag False matrix
    diagDiff    = diagonalDifference matrix

  putStrLn $ mconcat ["Diag Sum         = ", show diagSum]
  putStrLn $ mconcat ["Opp Diag Sum     = ", show oppSumDiag]
  putStrLn $ mconcat ["Diag Difference  = ", show diagDiff]

