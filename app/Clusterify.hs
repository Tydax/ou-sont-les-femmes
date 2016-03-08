module Main where

import Types
import NameCluster
import Utils

displayClusters :: [Cluster] -> IO ()
displayClusters = mapM_ (putStrLn . show)

main :: IO ()
main = displayClusters . clusterify $ words "Armand Arnaud Theo Alex \
\ Adrien Tristan Alexandra Julien Ilhem Alyx Tristan Alexis Alexandre Theo Thibault Thomas"
{-
main = do putStrLn "List your names separated by spaces: "
          --names <- getLine
          --names <- "Armand Arnaud Theo Adrien Tristan Julien Ilhem"
          displayClusters . clusterify $ words names
-}