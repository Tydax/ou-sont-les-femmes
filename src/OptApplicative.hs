{- |
  Module      :  $Header$
  Description :  Describes the options and parsers used for app options
  Copyright   :  None
  License     :  None

  Maintainer  :  tydax@protonmail.ch
  Stability   :  experimental

  The $Header$ module describes all the options and parsers used in the program
  such as the arguments, the flags, etc.
-}
module OptApplicative (
  Command (VoiciLesFemmes, GroupeLesNoms),
  Format(Normal, Csv),
  GlobalOptions(GlobalOptions),
  globalOptions,
  glnOptions,
  optMain,
  vlfOptions,
  test
) where

import Options.Applicative

import Types

-- |Global options used in the executable.
data GlobalOptions = GlobalOptions
  { optInput :: String -- Reads a file as a input
  , optOutput :: String -- Writes output to a file
  , optCsvFlag :: Format -- Defines output format
  , optCommand :: Command -- Command to execute
  , arguments :: [Name] -- List of 'Types.Name's to use
  } deriving (Show)

-- |Defines the export format used.
data Format = Normal | Csv deriving (Show)

-- |List of commands available in the executable.
data Command
  = VoiciLesFemmes VLFOptions
  | GroupeLesNoms GLNOptions
  deriving (Show)

-- |Options associated with the 'VoiciLesFemmes' command.
data VLFOptions = VLFOptions
  { optBase :: String
  } deriving (Show)

-- |Options associated with the 'GroupeLesNoms' command.
data GLNOptions = GLNOptions
  { optDist :: Int
  } deriving (Show)

-- |Parser for the 'VLFOptions' command.
vlfOptions :: Parser VLFOptions
vlfOptions = VLFOptions
  <$> optional $ strOption
      ( long "base"
     <> short 'b'
     <> metavar "BASEFILE"
     <> help "Uses BASEFILE as a base of gendered names" )

-- |Parser for 'GLNOptions' command.
glnOptions :: Parser GLNOptions
glnOptions = GLNOptions
  <$> optional $ option auto
      ( long "distance"
     <> short 'd'
     <> metavar "DIST"
     <> help "Uses DIST as valeur discriminatoire for the clusters of names" )

-- |Parser for the 'GlobalOptions' of the program.
globalOptions :: Parser GlobalOptions
globalOptions = GlobalOptions
  <$> optional $ strOption
      ( long "input"
     <> short 'i'
     <> metavar "IFILE"
     <> help "Reads IFILE as an input" )
  <*> optional $ strOption
      ( long "output"
     <> short 'o'
     <> metavar "OFILE"
     <> help "Writes output to OFILE" )
  <*> flag Normal Csv
      ( long "csv"
     <> help "Allows CSV export" )
  <*> subparser
      ( command "voici-les-femmes" (info
        ( VoiciLesFemmes <$> vlfOptions )
        ( progDesc "Assigns a gender to each NAME" ))
     <> command "groupe-les-noms" (info
        ( GroupeLesNoms <$> glnOptions )
        ( progDesc "Creates clusters of NAMES based on likeness" ))
      )
  <*> many
      ( argument str
        ( metavar "NAMES..." ) )

-- TEST
test :: GlobalOptions -> IO ()
test = putStrLn . show

-- |Parses all the options and call the entry function.
optMain :: IO ()
optMain = execParser opts >>= test
  where
    opts = info (helper <*> globalOptions)
      ( fullDesc
     <> progDesc "Multiple tools for name classification purposes"
     <> header "ou-sont-les-femmes-? - name classification tools")
