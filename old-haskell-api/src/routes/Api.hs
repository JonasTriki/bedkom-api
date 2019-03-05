module Api
  ( Api
  , apiHandler
  ) where

import           Servant (Server)
import           Users   (UsersAPI, usersHandler)

type Api = UsersAPI

apiHandler :: Server Api
apiHandler = usersHandler
