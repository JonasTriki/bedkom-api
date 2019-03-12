{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Login
  ( LoginAPI
  , loginHandler
  ) where

import           ApiResponse
import           Data.Aeson         (FromJSON, ToJSON, Value (String))
import           Data.Aeson.Types
import           Data.Text          (Text)
import           GHC.Generics       (Generic)
import           RegisterUserBody   (RegisterUserBody)
import           Semester2
import           Servant
import           Web.FormUrlEncoded (FromForm)

{-- currentSemester :: IO Semester
currentSemester = do
  let (year, month, day) = getCurrentTime >>= return . toGregorian . utctDay
  Autumn year--}
data LoginInfo = LoginInfo
  { username :: Text
  , password :: Text
  } deriving (Generic)

instance FromForm LoginInfo

instance ToJSON LoginInfo

type LoginAPI = "login" :> ReqBody '[ FormUrlEncoded] LoginInfo :> Post '[ JSON] ApiResponse

loginHandler :: LoginInfo -> Handler ApiResponse
loginHandler info = login (username info) (password info)
  where
    login username password = return $ ApiResponse "ok" emptyObject
