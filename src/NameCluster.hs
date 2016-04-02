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
  computeAverageDistance,
  computeAverageDistances,
  distanceFromCluster,
  formulaDistance,
  reclusterify,
  reevaluateCentre,
  reevaluateCentres
) where

import Types
import Utils
import Debug.Trace

-- |Computes the distance between a Name and a 'Type.Cluster'.
distanceFromCluster :: Name -> Cluster -> Distance
distanceFromCluster name (Cluster _ ct) = distance name ct

{-|
  Describes the formula used to evaluate the maximum distance that a word must
  be from the centre of a cluster to be considered as being part of the cluster.
-}
formulaDistance :: Name -> Name -> Distance
formulaDistance n1 n2 = max (length n1) (length n2) `div` 2

{-|
  Adds the specified name to one of the clusters or creates a new cluster
  if the distances to the centre of the clusters are greater than the maximum
  distance possible.
-}
addToCluster :: Name -> [Cluster] -> [Cluster]
addToCluster n [] = [Cluster [n] n]
addToCluster n (c:cs)
  | distanceFromCluster n c <= formulaDistance n ct = (Cluster (n:ns) ct):cs
  | otherwise = c:addToCluster n cs
  where
    Cluster ns ct = c

{-|
  Distributes the specified names in the specified clusters,
  using the 'addToCluster' function.
-}
clusterifyNames :: [Name] -> [Cluster] -> [Cluster]
clusterifyNames [] cs = cs
clusterifyNames (n:ns) cs = addToCluster n $ clusterifyNames ns cs

{-|
  Computes the arithmetic mean of the distances of the specified centre
  to all the names.
-}
computeAverageDistance :: [Name] -> Centre -> (Distance, Centre)
computeAverageDistance ns ct =
  let
    filteredns = filter (ct /=) ns -- Exclude its own name
    dist = mean $ map (distance ct) filteredns
  in (dist, ct)

{-|
  Computes the average distance of all the names to each name as a centre.
  The average distance is associated with each name to help finding out which
  one is the best pseudo-centre to use.
-}
computeAverageDistances :: [Name] -> [(Distance, Centre)]
computeAverageDistances ns = map (computeAverageDistance ns) ns

{-|
  Reevaluates the centre of the specified cluster.
  To do so, the function computes the average distance of each name to all the
  other names, and takes as a pseudo-centre the name with the shortest distance
  to every name.
-}
reevaluateCentre :: Cluster -> Cluster
reevaluateCentre (Cluster (n:[]) ct) = Cluster [n] ct
reevaluateCentre (Cluster ns _) =
  let
    -- Compute the average distance for each name
    cts = computeAverageDistances ns
    mindist = minimum $ map fst cts -- Get the minimum distance
    Just ct = lookup mindist cts
    -- Get the name associated to that minimum distance
  in
    Cluster ns ct


-- |Reevaluates the centre of every specified clusters.
reevaluateCentres :: [Cluster] -> [Cluster]
reevaluateCentres = map reevaluateCentre

{-|
  Checks that the specified cluster is still consistent considering that each
  time a name has been added to it, the pseudo-centre might change.
  The output is a list of names that were taken out of their clusters and that
  needs to be clusterified again.
-}
checkCluster :: Cluster -> [Name]
checkCluster (Cluster [] _) = []
checkCluster c
  | distanceFromCluster n c > formulaDistance n ct = n:recursive
  | otherwise = recursive
  where
    Cluster (n:ns) ct = c
    recursive = checkCluster (Cluster ns ct)

{-|
  Checks that all clusters are still consistent calling the 'checkCluster'
  function.
-}
checkClusters :: [Cluster] -> [(Cluster, [Name])]
checkClusters [] = []
checkClusters (c:cs) =
  let
    excluded = checkCluster c
    newc =
      case excluded of
        [] -> c
        _  ->
          -- Cleansing the cluster from excluded names if relevant
          let Cluster ns ct = c
          in Cluster (filter (flip notElem excluded) ns) ct
  in
    (newc, excluded):checkClusters cs

{-|
  Reclusterifies the specified clusters until no names are exluded from the
  clusters, reevaluating the centres (with 'reevaluateCentres') and checking
  that the clusters are still consistent at each call (with 'checkClusters').
-}
reclusterify :: [Cluster] -> [Name] -> [Cluster]
reclusterify cs [] = cs
reclusterify cs ns =
  let
    newcs = reevaluateCentres $ clusterifyNames ns cs
    (updatedcs, excluded) = unzip (checkClusters newcs)
  in reclusterify updatedcs (concat excluded)

{-|
  An alias for the 'reclusterify' function used as an entry point for the
  recursion.
-}
clusterify :: [Name] -> [Cluster]
clusterify = reclusterify [] . rmdups
