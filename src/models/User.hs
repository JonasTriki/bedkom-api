{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module User
  ( User
  ) where

import           Data.Text        (Text)

import           Control.Lens
import           InformaticsStudy (InformaticsStudy)

data User = User
  { _id           :: Text
  , _firstName    :: Text
  , _lastName     :: Text
  , _email        :: Text
  , _studyProgram :: InformaticsStudy
  , _year         :: Int
  , _role         :: Text
  } deriving (Show, Eq)

makeLenses ''User
