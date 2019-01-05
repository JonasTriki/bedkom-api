{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Scrapper where

import Control.Lens hiding (re)
import Control.Monad (sequence)
import Data.Aeson (toJSON)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy as BL
import Data.Ini hiding (sections)
import Data.List (find)
import qualified Data.Map as Map
import Data.Maybe (fromJust)
import qualified Data.Text as T
import qualified Data.Text.Lazy as L
import Data.Text.Lazy.Builder (toLazyText)
import Data.Text.Lazy.Encoding
import HTMLEntities.Decoder
import Network.Wreq
import qualified Network.Wreq.Session as Sess
import Text.HTML.TagSoup
import Text.Regex.TDFA

readCredentials = do
  iniFile <- readIniFile "me.ini"
  let feidename = iniFile >>= lookupValue "DEFAULT" "feidename"
      password = iniFile >>= lookupValue "DEFAULT" "password"
  case sequence [feidename, password] of
    Right [fn, pwd] -> return (fn, pwd) -- Is tuple needed?
    Left err -> error err

nameValuePair tag = (fromAttrib "name" tag, fromAttrib "value" tag)

toByteString = encodeUtf8 . toLazyText . htmlEncodedText . L.toStrict . decodeUtf8 . renderTags

toFormParams :: [(BL.ByteString, BL.ByteString)] -> [FormParam]
toFormParams = map (\(k, v) -> BL.toStrict k := v)

toParams :: [(BL.ByteString, BL.ByteString)] -> Options
toParams = foldl (\acc (k, v) -> acc & param ((L.toStrict . decodeUtf8) k) .~ [(L.toStrict . decodeUtf8) v]) defaults

fetchStudy :: Sess.Session -> IO ()
fetchStudy sess = do
  loginRes <- Sess.get sess "https://fsweb.no/studentweb/login.jsf?inst=FSUIB"
  (feidename, password) <- readCredentials
  -- First we fetch fields we need to send to login
  let extraRe = "mojarra\\.jsfcljs\\(.*{'(.*)':'(.*)'}.*\\)"
      feideModuleBox =
        takeWhile (~/= "</section>") . dropWhile (~/= "<section data-flap-name=feide>") . parseTags $
        loginRes ^. responseBody
      tags = map nameValuePair $ filter (~== "<input type=hidden>") feideModuleBox
      [_, aKey, aVal] = getAllTextSubmatches $ toByteString feideModuleBox =~ extraRe :: [BL.ByteString]
      pairs = tags ++ [(aKey, aVal)]
      fields = toFormParams pairs
  -- Then we send the post request to the login jsf
  resp <- Sess.customHistoriedPayloadMethodWith "POST" defaults sess "https://fsweb.no/studentweb/login.jsf" fields
  print $ resp ^. hrFinalRequest
  {-- loginPost <- Sess.post sess "https://fsweb.no/studentweb/login.jsf" fields
  let tags = parseTags $ loginPost ^. responseBody
      asLen = fromAttrib "value" . fromJust $ find (~== TagOpen "" [("name", "asLen")]) tags
      authState = fromAttrib "value" . fromJust $ find (~== TagOpen "" [("name", "AuthState")]) tags
  -- Now we assemble the login post payload
      loginPostPayload =
        [ "asLen" := asLen
        , "AuthState" := authState
        , "org" := "uib.no"
        , "has_js" := "true"
        , "inside_iframe" := "0"
        , "feidename" := feidename
        , "password" := password
        ]
  -- B.writeFile "test.html" $ BL.toStrict $ loginPost ^. responseBody
  -- print loginPostPayload --}
  print "ok"

scrap :: IO ()
scrap = do
  sess <- Sess.newSession
  fetchStudy sess