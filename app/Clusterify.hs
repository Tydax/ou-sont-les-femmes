module Main where

import qualified Data.ByteString.Lazy as LazyBS
import qualified System.IO as Sys

import CSVPlayer
import Types
import NameCluster
import Utils

-- |Clusterifies the specifed names and converts it into a String.
stringifyClusters :: [String] -> String
stringifyClusters ns = (show . clusterify) ns

-- |Clusterifies the specified names and encodes the result into a CSV formatted String.
stringifyCSV :: [String] -> LazyBS.ByteString
stringifyCSV ns = (convertClustersToCSVString . clusterify) ns

-- |A bunch of hardcoded values used as a quick test.
hardcodedValues :: String
hardcodedValues = "Armand Arnaud Theo Alex \
\ Adrien Tristan Alexandra Julien Juliette Ilhem Alyx Tristan Alexis Alexandre Theo Thibault Thomas \
\ Armande Armando Alexia"

-- |Uses hard-coded values as a test for clusterification.
displayHardCodedValues :: Sys.Handle -> IO ()
displayHardCodedValues h = (LazyBS.hPut h . stringifyCSV . words) hardcodedValues

-- |Uses input as a list of names for clusterification.
displayInputValues :: IO ()
displayInputValues =
  do
    Sys.putStrLn "List your names separated by spaces: "
    interact (show . stringifyCSV . words)

writeToFile :: (Sys.Handle -> IO ()) -> IO ()
writeToFile input =
  do
    Sys.withFile "out/clusters.csv" Sys.WriteMode input

main :: IO ()
main = writeToFile displayHardCodedValues