{- |
  Module      :  $Header$
  Description :  Describes the functions related to the clusterification of names
  Copyright   :  None
  License     :  None

  Maintainer  :  tydax@protonmail.ch
  Stability   :  unstable

  The $Header$ module describes the different functions used to create cluster
  of names which are similar.
-}
module NameCluster (
  addToCluster,
  clusterify,
  distanceFromCluster,
  evaluateCentre,
  formulaDistance
) where

import Types
import Utils

{-|
  The 'addToCluster' function adds the specified name to one of the clusters or creates a new cluster
  if the distance is greater than all the others.
-}
addToCluster :: Name -> Distance -> [Cluster] -> [Cluster]
addToCluster n _ [] = [Cluster [n] n]
addToCluster n d (c:cs)
  | distanceFromCluster n c <= d = let Cluster ns ct = c
                                   in (Cluster ns $ evaluateCentre $ c):cs
  | otherwise = c:(addToCluster n d cs)

-- |The 'clusterify' function distributes the specified names in clusters, using the 'addToCluster' function.
clusterify :: [Name] -> Distance -> [Cluster]
clusterify [] dist = []
clusterify (n:ns) dist = addToCluster n dist $ clusterify ns dist

-- |The 'distanceFromCluster' function computes the distance between a Name and a Cluster.
distanceFromCluster :: Name -> Cluster -> Int
distanceFromCluster name (Cluster _ ct) = distance name ct

-- |The 'evaluateCentre' function reevaluates the centre in the specified Cluster
evaluateCentre :: Cluster -> Centre
evaluateCentre (Cluster ns ct) = ct

formulaDistance :: Name -> Distance
formulaDistance n = length n `div` 3 