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
  distanceFromCluster,
  addToCluster,
  clusterify
) where

import Types
import Utils

-- |The 'distanceFromCluster' function computes the distance between a Name and a Cluster.
distanceFromCluster :: Name -> Cluster -> Int
distanceFromCluster name (n:_) = distance name n

{-|
  The 'addToCluster' function adds the specified name to one of the clusters or creates a new cluster
  if the distance is greater than all the others.
-}
addToCluster :: Name -> Distance -> [Cluster] -> [Cluster]
addToCluster n _ [] = [[n]]
addToCluster n d (c:cs)
  | distanceFromCluster n c <= d = (n:c):cs
  | otherwise = c:(addToCluster n d cs)

-- |The 'clusterify' function distributes the specified names in clusters, using the 'addToCluster' function.
clusterify :: [Name] -> Distance -> [Cluster]
clusterify [] dist = []
clusterify (n:ns) dist = addToCluster n dist $ clusterify ns dist