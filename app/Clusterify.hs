module Main where

import CSVPlayer
import Types
import NameCluster
import Utils

displayClusters :: [String] -> String
displayClusters ns = (show . clusterify) ns

displayCSV :: [String] -> String
displayCSV ns = (show . convertClustersToCSV . clusterify) ns

useHardCodedValues :: IO ()
useHardCodedValues = (putStrLn . displayCSV . words) "Armand Arnaud Theo Alex \
\ Adrien Tristan Alexandra Julien Juliette Ilhem Alyx Tristan Alexis Alexandre Theo Thibault Thomas \
\ Armande Armando Alexia"

useInputValues :: IO ()
useInputValues = do putStrLn "List your names separated by spaces: "
                    interact (displayCSV . words)

main :: IO ()
main = useHardCodedValues