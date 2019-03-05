{-# LANGUAGE TypeApplications #-}

module Main where

import qualified Data.Aeson                      as Aeson

import           AWSLambda.Events.APIGateway     (apiGatewayMain)
import           AWSLambda.Events.APIGateway.Wai (fromWai)
import           Data.Convertible                (convert)
import           Data.Text                       (Text)

import           Lib
import           Network.Wai                     (Application)
import           Scrapper
import           System.Environment              (lookupEnv)

local :: IO ()
local = startApp

main :: IO ()
main = apiGateway app

apiGateway :: Application -> IO ()
apiGateway = apiGatewayMain . fromWai @Text @Text convert convert
