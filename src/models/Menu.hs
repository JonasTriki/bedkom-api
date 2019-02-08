{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Menu
  ( Menu
  ) where

import           Data.Text    (Text)

import           Control.Lens
import           FoodEntry    (FoodEntry)

data Menu = Menu
  { _id          :: Text
  , _name        :: Text
  , _foodEntries :: [FoodEntry]
  } deriving (Show, Eq)

makeLenses ''Menu
