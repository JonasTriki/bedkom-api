module LastAuthorized where

import           Data.Text (Text)

type UserId = Text

isUserAuthorized :: UserId -> IO Bool
isUserAuthorized uid = do
  print "123"
  return False
