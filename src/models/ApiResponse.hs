{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeSynonymInstances #-}

module ApiResponse
  ( ApiResponse
  , ApiResponseText
  ) where

import           Data.Text     (Text)

import           Control.Lens
import           Data.Aeson
import           Data.Aeson.TH
import           GHC.Generics

data ApiResponse a = ApiResponse
  { _status :: Text
  , _data   :: a
  } deriving (Show, Generic)

type ApiResponseText = ApiResponse Text

instance ToJSON ApiResponseText

instance FromJSON ApiResponseText
