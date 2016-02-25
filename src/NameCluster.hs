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
addToCluster :: Name -> [Cluster] -> [Cluster]
addToCluster n [] = [Cluster [n] n]
addToCluster n (c:cs)
  | distanceFromCluster n c <= formulaDistance n ct = (Cluster ns $ evaluateCentre $ c):cs
  | otherwise = c:(addToCluster n cs)
  where
    Cluster ns ct = c

-- |The 'clusterify' function distributes the specified names in clusters, using the 'addToCluster' function.
clusterify :: [Name] -> [Cluster]
clusterify [] = []
clusterify (n:ns) = addToCluster n $ clusterify ns

-- |The 'distanceFromCluster' function computes the distance between a Name and a Cluster.
distanceFromCluster :: Name -> Cluster -> Int
distanceFromCluster name (Cluster _ ct) = distance name ct

-- |The 'evaluateCentre' function reevaluates the centre in the specified Cluster
evaluateCentre :: Cluster -> Centre
evaluateCentre (Cluster ns ct) = ct

{-|
  The 'formulaDistance' function describes the formula used to evaluate the max distance that a word must be from
  the centre of a cluster to be considered as being part of the cluster.
-}
formulaDistance :: Name -> Name -> Distance
formulaDistance n1 n2 = min (length n1) (length n2) `div` 2