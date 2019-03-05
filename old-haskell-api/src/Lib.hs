{-# LANGUAGE OverloadedStrings #-}

module Lib
  ( startApp
  , app
  ) where

import           Api                         (Api, apiHandler)
import           Control.Monad.IO.Class      (liftIO)
import           Data.Aeson
import           Data.Aeson.TH
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Network.Wai.Logger          (withStdoutLogger)
import           Schema
import           Servant

port :: Int
port = 8080

startApp :: IO ()
startApp = do
  putStrLn $ "Running API on localhost @ port " ++ show port
  -- putStrLn "Migrating DB..."
  -- migrateDB
  -- putStrLn "Done!"
  withStdoutLogger $ \aplogger -> do
    let settings = setPort port $ setLogger aplogger defaultSettings
    runSettings settings app

app :: Application
app = serve api server

api :: Proxy Api
api = Proxy

server :: Server Api
server = apiHandler
