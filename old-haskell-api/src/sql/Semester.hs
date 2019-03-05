{-# LANGUAGE TemplateHaskell #-}

module Semester where

import           Database.Persist.TH

data Semester
  = Spring
  | Autumn
  deriving (Show, Read, Eq)

derivePersistField "Semester"
