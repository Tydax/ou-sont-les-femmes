{- |
  Module      :  $Header$
  Description :  Describes the functions used to load and write CSV files
  Copyright   :  None
  License     :  None

  Maintainer  :  tydax@protonmail.ch
  Stability   :  unstable

  The $Header$ module describes the different functions used to load the
  database files under the CSV format. It also provides functions to produce
  the .CSV files.
-}
module CSVPlayer (
  clusterHeader,
  convertClustersToCSVString,
  genderedNameHeader,
  toClusterRecords,
  toClusterRecordsAll,
  writeCSVFile
) where

import Control.Monad
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LazyBS
-- import Data.Vector

import Data.Csv

import Types

-- |Implementation of 'Data.Csv.ToNamedRecord' for a 'Types.ClusterRecord'.
instance ToNamedRecord ClusterRecord where
  toNamedRecord (ClusterRecord (ct, n)) =
    let h1:h2:_ = clusterHeader
    in namedRecord [h1 .= ct, h2 .= n]

-- |Implementation of 'Data.Csv.DefaultOrdered' for a 'Types.ClusterRecord'.
instance DefaultOrdered ClusterRecord where
  headerOrder _ = header clusterHeader

-- |Gets the headers for the cluster .CSV files.
clusterHeader :: [BS.ByteString]
clusterHeader = [toField "Pseudocentre", toField "Name"]

{-|
  Converts a 'Type.Cluster' to a list of 'Types.ClusterRecord's for CSV
  conversion.
-}
toClusterRecords :: Cluster -> [ClusterRecord]
toClusterRecords (Cluster ns ct) = map (\n -> ClusterRecord (ct, n)) ns

{-|
  Converts a list of 'Type.Cluster's to a list of 'Types.ClusterRecord's
  for CSV conversion.
-}
toClusterRecordsAll :: [Cluster] -> [ClusterRecord]
toClusterRecordsAll = concat . map toClusterRecords

-- |Converts a list of 'Type.Cluster's to a CSV.
convertClustersToCSVString :: [Cluster] -> LazyBS.ByteString
convertClustersToCSVString = encodeDefaultOrderedByName . toClusterRecordsAll

-- |Implementation of 'Data.Csv.ToField' for a 'Types.Gender'.
instance ToField Gender where
  toField Female = toField "Female"
  toField Other  = toField "Other"

-- |Implementation of 'Data.Csv.FromRecord' for a 'Types.GenderedName'.
instance FromRecord GenderedName where
  parseRecord r
    | length r >= 2 =
      let
        f = \x -> if x == 'f'
          then Female
          else Other
        gender = f <$> (r .! 1)
        tuple = (,) <$> (r .! 0) <*> gender
      in GenderedName <$> tuple
    | otherwise = mzero

-- |Implementation of 'Data.Csv.ToNamedRecord' for a 'Types.GenderedName'.
instance ToNamedRecord GenderedName where
  toNamedRecord (GenderedName (n, g)) =
    let h1:h2:_ = genderedNameHeader
    in namedRecord [h1 .= n, h2 .= g]

-- |Implementation of 'Data.Csv.DefaultOrdered' for a 'Types.GenderedName'.
instance DefaultOrdered GenderedName where
  headerOrder _ = header genderedNameHeader

-- |Gets the headers for the gendered name .CSV file.
genderedNameHeader = [toField "Name", toField "Gender"]

{-|
  Writes the specified 'Data.ByteString.Lazy' to a .csv file, using the given
  name.
-}
writeCSVFile :: (DefaultOrdered a, ToNamedRecord a) => FilePath -> [a] -> IO ()
writeCSVFile n =
  LazyBS.writeFile ("out/" ++ n ++ ".csv") . encodeDefaultOrderedByName

-- |Loads the CSV gendered name base using the specified file path.
loadGenderedBase :: FilePath -> IO (Maybe [GenderedName])
loadGenderedBase fp =
  do
    file <- LazyBS.readFile fp
    let list = decode file :: Either String [GenderedName]
    res <- case list of
      Left _  -> Nothing
      Right _ -> Just list
    return res
