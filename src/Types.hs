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
  ClusterRecord(ClusterRecord),
  Distance,
  Name
) where

-- |Pseudocentre of a 'Cluster'.
type Centre = Name

-- |A group containing 'Name's which distance  is short.
data Cluster = Cluster [Name] Centre deriving (Show)

-- |A record in CSV for a 'Name' of a 'Cluster'.
newtype ClusterRecord = ClusterRecord (Centre, Name) deriving (Show)

{-|
  The Levenshtein distance between two strings, using an integer to represent how many letter
  operations are needed to transform a word into the other word.
-}
type Distance = Int

-- |A name.
type Name = String

{-|
  The 'Gender' type is used to represent the gender of a name.
  'Other' is used for the other names that are not considered female names just for the sake of non-binarity.
-}
data Gender = Female | Other
