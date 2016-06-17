{- |
  Module      :  $Header$
  Description :  Describes the functions related to the clusterification of names
  Copyright   :  None
  License     :  None

  Maintainer  :  tydax@protonmail.ch
  Stability   :  unstable

  The $Header$ module describes the different functions used to eveluate the
  gender of a name.
-}
module NameGender (
  findGenderBase
) where

import Types

{-|
  Finds a gender for the specified list of names using the specified base.
  The result can be a 'Tyoes.GenderedName' if the name was found in the
  specified base, or just the name else.
-}
findGenderBase :: [GenderedName] -> [Name] -> [Either GenderedName Name]
findGenderBase base ns =
  let
    untainedBase = [x | GenderedName x <- base]
    look name =
      let
        res = lookup name untainedBase
      in
        case res of
          Just gender-> Left (GenderedName (name, gender))
          Nothing -> Right name
  in map look ns
