{-# LANGUAGE TemplateHaskell #-}

module UserRole where

import           Database.Persist.TH

data UserRole
  = Student
  | Company
  | Bedkom
  | BedkomAdmin
  | SuperAdmin
  deriving (Show, Read, Eq)

derivePersistField "UserRole"
