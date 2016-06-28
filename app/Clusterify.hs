module Main where

import qualified Data.ByteString.Lazy as LazyBS
import Data.Time.Clock
import Data.Time.Format
import Data.Time.LocalTime
import System.IO
--import System.Locale

import CSVPlayer
import Types
import NameCluster
import NameGender
import Utils

-- |Clusterifies the specifed names and converts it into a String.
stringifyClusters :: [String] -> String
stringifyClusters ns = (show . clusterify) ns

{-|
  Clusterifies the specified names and encodes the result into a CSV formatted
  String.
-}
stringifyCSV :: [String] -> String
stringifyCSV ns = (show . convertClustersToCSVString . clusterify) ns

-- TEST
hardcodedValues :: [String]
hardcodedValues = words "Armand  Theo Alex Adrien Tristan Alexandra \
\ Arnaud Julien Juliette Ilhem Alyx Tristan Alexis Alexandre Theo Thibault Thomas \
\ Armande Armando Alexia"

-- TEST
displayHardCodedValues :: IO ()
displayHardCodedValues = (putStrLn . show . stringifyClusters) hardcodedValues

-- |Uses input as a list of names for clusterification.
displayInputValues :: IO ()
displayInputValues =
  do
    putStrLn "List your names separated by spaces: "
    interact (show . stringifyClusters . words)

-- |Clusterifies input values and writes the result to a new CSV file.
writeCSVFileInputValues :: IO ()
writeCSVFileInputValues =
  do
    putStrLn "List your names separated by spaces: "
    names <- getLine
    writeCSVFile "clusters" (toClusterRecordsAll . clusterify . words $ names)

-- TEST
writeCSVFileHardCodedValues :: IO ()
writeCSVFileHardCodedValues =
  do
    filename <- generateFilenameFromTime
    writeCSVFile filename (toClusterRecordsAll . clusterify $ hardcodedValues)

-- |Generates a file name from the current time.
generateFilenameFromTime :: IO String
generateFilenameFromTime =
  do
    timezone <- getCurrentTimeZone
    utcTime <- getCurrentTime
    -- Getting current time for filename
    let localTime = utcToLocalTime timezone utcTime
    -- Formatting time for filename
    let filename = formatTime defaultTimeLocale "%Y-%m-%d_%H-%M-%S" localTime
    return filename

main :: IO ()
-- main = displayHardCodedValues
main =
  let
    base = loadGenderedBase "data/db_all_names.csv"
    ns = ["Alexandre", "Diamant", "ChouDeBruxelles41", "Jackie"]
  in (putStrLn . show) (findGenderBase base ns)
