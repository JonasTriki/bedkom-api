{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Presentation
  ( Presentation
  ) where

import           Data.Text     (Text)

import           Company       (Company)
import           Control.Lens
import           Data.Aeson
import           Data.Aeson.TH
import           Menu          (Menu)
import           Semester2     (Semester2)
import           User          (User)

data Presentation = Presentation
  { _id            :: Text
  , _semester2     :: Semester2
  , _capacity      :: Int
  , _startTime     :: Int
  , _endTime       :: Int
  , _company       :: Company
  , _menu          :: Maybe Menu
  , _organizers    :: [User]
  , _registrations :: [User]
  , _waitlist      :: [User]
  } deriving (Show, Eq)

makeLenses ''Presentation
