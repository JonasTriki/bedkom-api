{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Company
  ( Company
  ) where

import           Data.Text    (Text)

import           Control.Lens

data Company = Company
  { _id          :: Text
  , _name        :: Text
  , _description :: Text
  , _website     :: Int
  , _bannerUrl   :: Maybe Text
  } deriving (Show, Eq)

makeLenses ''Company
