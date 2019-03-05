{-# LANGUAGE DeriveGeneric #-}

module RegisterUserBody
  ( RegisterUserBody
  ) where

import           Data.Text     (Text)

import           Control.Lens
import           Data.Aeson
import           Data.Aeson.TH
import           GHC.Generics  (Generic)

data RegisterUserBody = RegisterUserBody
  { _id   :: Text
  , _name :: Text
  } deriving (Generic)

instance ToJSON RegisterUserBody

instance FromJSON RegisterUserBody
