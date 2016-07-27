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
  GlobalOptions(GlobalOptions),
  Format(Normal, Csv),
  Command (VoiciLesFemmes, GroupeLesNoms),
  globalOptions,
  glnOptions,
  vlfOptions
) where

import Options.Applicative

-- |Global options used in the executable.
data GlobalOptions = GlobalOptions
  { optInput :: String -- Reads a file as a input
  , optOutput :: String -- Writes output to a file
  , optCsvFlag :: Format --
  , optCommand :: Command
  , arguments :: [Name]
  }

-- |Defines the export format used.
data Format = Normal | Csv

-- |List of commands available in the executable.
data Command
  = VoiciLesFemmes VLFOptions
  | GroupeLesNoms GLNOptions

-- |Options associated with the 'VoiciLesFemmes' command.
data VLFOptions = VLFOptions
  { optBase :: String
  }

-- |Options associated with the 'GroupeLesNoms' command.
data GLNOptions = GLNOptions
  { optDist :: Int
  }

-- |Parser for the 'VLFOptions'.
vlfOptions :: Parser VLFOptions
vlfOptions = VLFOptions
  <$> strOption
      ( long "base"
     <> short "b"
     <> metavar "BASEFILE"
     <> help "Uses BASEFILE as a base of gendered names" )

glnOptions :: Parser GLNOptions
glnOptions = GLNOptions
  <$> intOption -- TODO check this
      ( long "distance"
     <> short "d"
     <> metavar "DIST"
     <> help "Uses DIST as valeur discriminatoire for the clusters of names" ) -- TODO


-- |Parser for the 'GlobalOptions' of the program.
globalOptions :: Parser GlobalOptions
globalOptions = GlobalOptions
  <$> strOption
      ( long "input"
     <> short 'i'
     <> metavar "IFILE"
     <> help "Reads IFILE as an input" )
  <*> strOption
      ( long "output"
     <> short "o"
     <> metavar "OFILE"
     <> help "Writes output to OFILE" )
  <*> subparser
      ( command "voici-les-femmes" (info vlfOptions
        ( progDesc "Assigns a gender to each NAME" ))
     <> command "groupe-les-noms" (info glnOptions
        ( progDesc "Creates clusters of NAMES based on /ressemblance/ " )) -- TODO
      )
  <*> flag Normal Csv
      ( long "csv"
     <> help "Allows CSV export" )
  <*> many
      ( argument
        ( str
          ( metavar "NAMES..." ) ) )
