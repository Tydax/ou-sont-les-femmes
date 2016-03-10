module Main where

import Types
import NameCluster
import Utils

displayClusters :: [Cluster] -> IO ()
displayClusters = mapM_ (putStrLn . show)

useHardCodedValues :: IO()
useHardCodedValues = displayClusters . clusterify $ words "Armand Arnaud Theo Alex \
\ Adrien Tristan Alexandra Julien Juliette Ilhem Alyx Tristan Alexis Alexandre Theo Thibault Thomas \
\ Armande Armando Alexia"

useInputValues :: IO()
useInputValues = do putStrLn "List your names separated by spaces: "
                    names <- getLine
                    displayClusters . clusterify $ words names

main :: IO ()
main = useHardCodedValues
