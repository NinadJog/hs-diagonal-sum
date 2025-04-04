{-
Contains general-purpose functions for commonly used IO and non-IO tasks.
-}
module Utilities (
  if',
  printDoubles, printList
) where

import Text.Printf

-------------------------------------------------------------------------------
-- A helper function for if-then-else
if' :: Bool -> a -> a -> a
if' True x _  = x
if' False _ y = y

-------------------------------------------------------------------------------
{- Prints an array of doubles with 6 digits after the decimal point, 
   with one number per line
-}
printDoubles :: [Double] -> IO ()
printDoubles = mapM_ $ printf "%.6f\n"

-------------------------------------------------------------------------------
-- Print a list of anything that implements show, with one element on each line
printList :: Show(a) => [a] -> IO ()
printList = mapM_ (putStrLn . show)

-------------------------------------------------------------------------------