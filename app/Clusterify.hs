module Main where

import Data.Time.Clock
import Data.Time.Format
import Data.Time.LocalTime
import System.IO
--import System.Locale

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
    timezone <- getCurrentTimeZone
    utcTime <- getCurrentTime
    let localTime = utcToLocalTime timezone utcTime -- Getting current time for filename
    let filename = formatTime defaultTimeLocale "%Y-%m-%dh%H-%M-%S" localTime -- Formatting time for filename
    writeCSVFile filename (toClusterRecordsAll . clusterify $ hardcodedValues)


-- |Gets the date and time of day in the current timezone.
--getDateAndTime :: TimeOfDay
--getDateAndTime =
--  do

--    return (timeOfDay)
--  in
--    timeOfDay

main :: IO ()
main = writeCSVFileHardCodedValues
