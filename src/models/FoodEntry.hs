{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module FoodEntry
  ( FoodEntry
  ) where

import           Data.Text    (Text)

import           Control.Lens

data FoodEntry = FoodEntry
  { _id      :: Text
  , _name    :: Text
  , _details :: Text
  } deriving (Show, Eq)

makeLenses ''FoodEntry
