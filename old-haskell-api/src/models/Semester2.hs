module Semester2
  ( Semester2(..)
  ) where

type Year = Int

data Semester2
  = Autumn Year
  | Spring Year
  deriving (Eq)

instance Show Semester2 where
  show (Autumn year) = "Vår " ++ show year
  show (Spring year) = "Høst " ++ show year
