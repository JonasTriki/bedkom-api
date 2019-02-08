{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Dot
  ( Dot
  ) where

import           Data.Text        (Text)

import           Control.Lens
import           InformaticsStudy (Semester)

data Dot = Dot
  { _id       :: Text
  , _userId   :: Text
  , _semester :: Semester
  , _dots     :: Int
  } deriving (Show, Eq)

makeLenses ''Dot
