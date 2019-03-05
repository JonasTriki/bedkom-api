{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module Users
  ( UsersAPI
  , usersHandler
  ) where

import           Login
import           Register
import           Servant

type UsersAPI = "users" :> RegisterAPI :<|> "users" :> LoginAPI

usersHandler :: Server UsersAPI
usersHandler = registerHandler :<|> loginHandler
