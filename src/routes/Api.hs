module Api
  ( Api
  , apiHandler
  ) where

import           Users (UsersAPI, usersHandler)

type Api = UsersAPI

apiHandler = usersHandler
