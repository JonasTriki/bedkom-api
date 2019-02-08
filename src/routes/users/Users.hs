{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module Users
  ( UsersAPI
  , usersHandler
  ) where

import           Login
import           Register
import           Servant

type UsersAPI = "users" :> RegisterAPI :<|> LoginAPI

usersHandler = registerHandler :<|> loginHandler
