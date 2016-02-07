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
  distance
) where

import Types

{-|
  The 'distance' computes the Leveinshtein distance between two strings,
  using an integer to represent how many letter operations is needed to transform
  a word into the other word.
-}
distance :: String -> String -> Distance
distance = levenshteinDistance defaultEditCosts