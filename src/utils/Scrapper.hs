module Scrapper where

import Network.HTTP

openURL :: String -> IO String
openURL x = getResponseBody =<< simpleHTTP (getRequest x)

main :: IO ()
main = do
  src <- openURL "http://wiki.haskell.org/Haskell"
  writeFile "temp.htm" src
