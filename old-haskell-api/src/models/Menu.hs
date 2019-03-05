{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Menu
  ( Menu
  ) where

import           Data.Text     (Text)

import           Control.Lens
import           Data.Aeson
import           Data.Aeson.TH
import           FoodEntry     (FoodEntry)

data Menu = Menu
  { _id          :: Text
  , _name        :: Text
  , _foodEntries :: [FoodEntry]
  } deriving (Show, Eq)

makeLenses ''Menu
