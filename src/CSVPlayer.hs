{- |
  Module      :  $Header$
  Description :  Describes the functions used to load and write the CSV files
  Copyright   :  None
  License     :  None

  Maintainer  :  tydax@protonmail.ch
  Stability   :  unstable

  The $Header$ module describes the different functions used to load the database files
  under the CSV format. It also provides functions to produce the .CSV files.
-}
module CSVPlayer (
  toClusterRecords
) where

import Data.Csv

import Types

-- | Converts a Cluster to a list of ClusterRecords for CSV convertion.
toClusterRecords :: Cluster -> [ClusterRecord]
toClusterRecords (Cluster (n:ns) ct) =
  let newC = Cluster ns ct
  in (ct, n):toClusterRecords newC

instance ToRecord ClusterRecord where
  toRecord (ct, n) = record [toField ct, toField n]