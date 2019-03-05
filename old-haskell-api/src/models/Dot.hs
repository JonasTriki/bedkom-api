{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Dot
  ( Dot
  ) where

import           Data.Text     (Text)

import           Control.Lens
import           Data.Aeson
import           Data.Aeson.TH
import           Semester2     (Semester2)

data Dot = Dot
  { _id        :: Text
  , _userId    :: Text
  , _semester2 :: Semester2
  , _dots      :: Int
  } deriving (Show, Eq)

makeLenses ''Dot
