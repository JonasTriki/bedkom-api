{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Register
  ( RegisterAPI
  , registerHandler
  ) where

import           ApiResponse
import           Data.Aeson.Types (emptyObject)
import           GHC.Generics     (Generic)
import           RegisterUserBody (RegisterUserBody)
import           Servant

-- :> ReqBody '[ JSON] RegisterUserBody
type RegisterAPI = "register" :> Post '[ JSON] ApiResponse

registerHandler :: Handler ApiResponse
registerHandler = return $ ApiResponse "ok" emptyObject
