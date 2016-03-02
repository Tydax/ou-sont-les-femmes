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
  checkCluster,
  checkClusters,
  clusterify,
  clusterifyNames,
  distanceFromCluster,
  formulaDistance,
  reclusterify,
  reevaluateCentres
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
  | distanceFromCluster n c <= formulaDistance n ct = (Cluster (n:ns) ct):cs
  | otherwise = c:(addToCluster n cs)
  where
    Cluster ns ct = c

{-|
  The 'checkCluster' function checks that the specified cluster is still consistent considering that the centre of
  each cluster has been reevaluated each time that a name has been added to it.
  The output is a list of names that were taken out of their clusters and that need to be clusterified again.
-}
checkCluster :: Cluster -> [Name]
checkCluster (Cluster [] _) = []
checkCluster c
  | distanceFromCluster n c > formulaDistance n ct = (n:recursive)
  | otherwise = recursive
  where 
    Cluster (n:ns) ct = c
    recursive = checkCluster (Cluster ns ct)

-- |The 'checkClusters' function checks that all clusters are still consistent calling the 'checkCluster' function.
checkClusters :: [Cluster] -> [Name]
checkClusters [] = []
checkClusters (c:cs) = checkCluster c ++ checkClusters cs

-- |An alias for the 'reclusterify' function used as an entry point for the recursion.
clusterify :: [Name] -> [Cluster]
clusterify = reclusterify []

-- |The 'clusterifyNames' function distributes the specified names in the specified clusters, using the 'addToCluster' function.
clusterifyNames :: [Name] -> [Cluster] -> [Cluster]
clusterifyNames [] cs = cs
clusterifyNames (n:ns) cs = addToCluster n $ clusterifyNames ns cs

-- |The 'distanceFromCluster' function computes the distance between a Name and a Cluster.
distanceFromCluster :: Name -> Cluster -> Distance
distanceFromCluster name (Cluster _ ct) = distance name ct

{-|
  The 'formulaDistance' function describes the formula used to evaluate the max distance that a word must be from
  the centre of a cluster to be considered as being part of the cluster.
-}
formulaDistance :: Name -> Name -> Distance
formulaDistance n1 n2 = min (length n1) (length n2) `div` 2

{-|
  'reclusterify' is a recursive function checking at each call that the clusters are consistent
  using the 'checkClusters' function. If they are not consistent, 'reclusterify' calls itself again.
-}
reclusterify :: [Cluster] -> [Name] -> [Cluster]
reclusterify cs [] = cs
reclusterify cs ns =
  let
    newcs = reevaluateCentres $ clusterifyNames ns cs
    excluded = checkClusters newcs
  in reclusterify newcs excluded

-- | TODO The 'reevaluateCentre' function reevaluates the centre in the specified Cluster
reevaluateCentres :: [Cluster] -> [Cluster]
reevaluateCentres cs = cs
