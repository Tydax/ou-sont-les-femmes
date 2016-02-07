module Main where

import Types
import NameCluster
import Utils

displayClusters :: [Cluster] -> IO ()
displayClusters [] = putStrLn "End."
displayClusters (c:cs) = do putStr "Cluster: "
                            putStrLn $ show cs
                            displayClusters cs

main :: IO ()
main = do putStrLn "List your names separated by spaces: "
          names <- getLine
          displayClusters (clusterify (words names) 2)