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
  clusterHeader,
  convertClustersToCSV,
  toClusterRecords,
  toClusterRecordsAll
) where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LazyBS
import Data.Csv

import Types

-- |Implementation of ToRecord for a ClusteRecord.
instance ToNamedRecord ClusterRecord where
  toNamedRecord (ClusterRecord (ct, n)) =
    let h1:h2:_ = clusterHeader
    in namedRecord [h1 .= ct, h2 .= n]

instance DefaultOrdered ClusterRecord where
  headerOrder _ = header clusterHeader

-- |Gets the headers for the cluster .CSV files.
clusterHeader :: [BS.ByteString]
clusterHeader = [toField "Pseudocentre", toField "Name"]

-- |Converts a Cluster to a list of ClusterRecords for CSV conversion.
toClusterRecords :: Cluster -> [ClusterRecord]
toClusterRecords (Cluster ns ct) = map (\n -> ClusterRecord (ct, n)) ns

-- |Converts a list of Clusters to a list of ClusterRecords for CSV conversion.
toClusterRecordsAll :: [Cluster] -> [ClusterRecord]
toClusterRecordsAll = concat . map toClusterRecords

-- |Converts a list of Clusters to a CSV.
convertClustersToCSV :: [Cluster] -> LazyBS.ByteString
convertClustersToCSV = encodeDefaultOrderedByName . toClusterRecordsAll