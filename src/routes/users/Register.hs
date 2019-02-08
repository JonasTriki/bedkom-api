{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module Register
  ( RegisterAPI
  , registerHandler
  ) where

import           ApiResponse      (ApiResponse, ApiResponseText)
import           Data.Text        (Text)
import           RegisterUserBody (RegisterUserBody)
import           Servant

type RegisterAPI = "register" :> ReqBody '[ JSON] RegisterUserBody :> Post '[ JSON] ApiResponseText

registerHandler :: RegisterUserBody -> Handler ApiResponseText
registerHandler body = undefined
