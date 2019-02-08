{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module Login
  ( LoginAPI
  , loginHandler
  ) where

import           ApiResponse      (ApiResponse, ApiResponseText)
import           Data.Text        (Text)
import           RegisterUserBody (RegisterUserBody)
import           Servant

type LoginAPI = "login" :> Post '[ JSON] ApiResponseText

loginHandler :: Handler ApiResponseText
loginHandler = undefined
