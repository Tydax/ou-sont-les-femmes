{- |
  Module      :  $Header$
  Description :  Describes all base types used by the application
  Copyright   :  None
  License     :  None

  Maintainer  :  tydax@protonmail.ch
  Stability   :  experimental

  The $Header$ module describes all the types used in the application. Many of the types
  are here just to make the code easier to understand and read.
-}
module Types (
  Centre,
  Cluster(Cluster),
  Distance,
  Name,
  Gender(Female, Other)
) where

-- |The 'Centre' represents the centre of a cluster.
type Centre = Name

{-|
  The 'Cluster' type describes a group containing names which distance 
  is short.
-}
data Cluster = Cluster [Name] Centre deriving (Show)

-- |The 'ClusterRecord' represent one record in CSV for a name of a cluster.
type ClusterRecord = (Centre, Name) 

{-|
  The 'Distance' type describes the Levenshtein distance between two strings,
  using an integer to represent how many letter operations is needed to transform
  a word into the other word.
-}
type Distance = Int

-- |The 'Name' type describes a name.
type Name = String