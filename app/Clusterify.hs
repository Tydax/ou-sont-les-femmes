module Main where

import System.IO

import CSVPlayer
import Types
import NameCluster
import Utils

-- |Clusterifies the specifed names and converts it into a String.
stringifyClusters :: [String] -> String
stringifyClusters ns = (show . clusterify) ns

-- |Clusterifies the specified names and encodes the result into a CSV formatted String.
stringifyCSV :: [String] -> String
stringifyCSV ns = (show . convertClustersToCSVString . clusterify) ns

-- |A bunch of hardcoded values used as a quick test.
hardcodedValues :: [String]
hardcodedValues = words "Armand Arnaud Theo Alex \
\ Adrien Tristan Alexandra Julien Juliette Ilhem Alyx Tristan Alexis Alexandre Theo Thibault Thomas \
\ Armande Armando Alexia"

-- |Uses hard-coded values as a test for clusterification.
displayHardCodedValues :: IO ()
displayHardCodedValues = (putStrLn . show . stringifyCSV) hardcodedValues

-- |Uses input as a list of names for clusterification.
displayInputValues :: IO ()
displayInputValues =
  do
    putStrLn "List your names separated by spaces: "
    interact (show . stringifyCSV . words)

writeCSVFileInputValues :: IO ()
writeCSVFileInputValues =
  do
    putStrLn "List your names separated by spaces: "
    names <- getLine
    writeCSVFile "clusters" (toClusterRecordsAll . clusterify . words $ names)

writeCSVFileHardCodedValues :: IO ()
writeCSVFileHardCodedValues =
  do
    putStrLn "Generating clusters and writing to CSV file..."
    writeCSVFile "clusters" (toClusterRecordsAll . clusterify $ hardcodedValues)

main :: IO ()
main = writeCSVFileInputValues
