{- |
  Module      :  $Header$
  Description :  Describes the generic util functions
  Copyright   :  None
  License     :  None

  Maintainer  :  tydax@protonmail.ch
  Stability   :  provisional

  The $Header$ module describes the generic util functions.
-}
module Utils (
  distance,
  mean
) where

import Types
import Text.EditDistance

{-|
  The 'distance' computes the Leveinshtein distance between two strings,
  using an integer to represent how many letter operations is needed to transform
  a word into the other word.
-}
distance :: String -> String -> Distance
distance = levenshteinDistance defaultEditCosts

-- |The 'myMean' functions computes the arithmetic mean of the specified list of numbers, excluding one number.
mean :: [Int] -> Int
mean [] = 0
mean nbs = (sum nbs) `div` (length nbs)