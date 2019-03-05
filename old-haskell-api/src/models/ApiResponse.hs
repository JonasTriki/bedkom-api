{-# LANGUAGE DeriveGeneric #-}

module ApiResponse
  ( ApiResponse(..)
  ) where

import           Data.Text    (Text)

import           Data.Aeson
import           GHC.Generics

data ApiResponse = ApiResponse
  { _status :: Text
  , _data   :: Value
  } deriving (Eq, Show, Generic)

instance ToJSON ApiResponse

instance FromJSON ApiResponse
