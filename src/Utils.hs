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
  mean,
  rmdups
) where

import qualified Data.Set as Set

import Text.EditDistance

import Types


{-|
  Computes the Leveinshtein distance between two strings,
  using an integer to represent how many letter operations is needed to transform
  a word into the other word.
-}
distance :: String -> String -> Distance
distance = levenshteinDistance defaultEditCosts

-- |Computes the arithmetic mean of the specified list of numbers, excluding one number.
mean :: [Int] -> Int
mean [] = 0
mean nbs = (sum nbs) `div` (length nbs)

-- |Deletes the dupplicated values from the specified list.
rmdups :: Ord a => [a] -> [a]
rmdups = rmdups' Set.empty where
  rmdups' _ [] = []
  rmdups' a (b:c) =
    if Set.member b a then rmdups' a c
    else b : rmdups' (Set.insert b a) c
