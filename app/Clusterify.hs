module Main where

import CSVPlayer
import Types
import NameCluster
import Utils

-- |Clusterifies the specifed names and converts it into a String.
displayClusters :: [String] -> String
displayClusters ns = (show . clusterify) ns

-- |Clusterifies the specified names and encodes the result into a CSV formatted String.
displayCSV :: [String] -> String
displayCSV ns = (show . convertClustersToCSV . clusterify) ns

-- |Uses hard-coded values as a test for clusterification.
useHardCodedValues :: IO ()
useHardCodedValues = (putStrLn . displayCSV . words) "Armand Arnaud Theo Alex \
\ Adrien Tristan Alexandra Julien Juliette Ilhem Alyx Tristan Alexis Alexandre Theo Thibault Thomas \
\ Armande Armando Alexia"

-- |Uses input as a list of names for clusterification.
useInputValues :: IO ()
useInputValues = do putStrLn "List your names separated by spaces: "
                    interact (displayCSV . words)

main :: IO ()
main = useHardCodedValues