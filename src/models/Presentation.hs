{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Presentation
  ( Presentation
  ) where

import           Data.Text        (Text)

import           Company          (Company)
import           Control.Lens
import           InformaticsStudy (Semester)
import           Menu             (Menu)
import           User             (User)

data Presentation = Presentation
  { _id            :: Text
  , _semester      :: Semester
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
