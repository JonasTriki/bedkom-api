{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE QuasiQuotes          #-}

module Scrapper where

import           Control.Lens                 hiding (re)
import           Control.Monad                (sequence)
import           Data.Aeson                   (Array, toJSON)
import qualified Data.ByteString              as B
import qualified Data.ByteString.Char8        as C
import qualified Data.ByteString.Lazy         as BL
import           Data.Ini                     hiding (sections)
import qualified Data.Text                    as T
import qualified Data.Text.Lazy               as L
import           Data.Text.Lazy.Builder       (toLazyText)
import           Data.Text.Lazy.Encoding
import           HTMLEntities.Decoder
import           Network.Wreq
import qualified Network.Wreq.Session         as Sess
import           Text.HTML.TagSoup
import           Text.RE.TDFA.ByteString.Lazy

readCredentials = do
  iniFile <- readIniFile "me.ini"
  let feidename = iniFile >>= lookupValue "DEFAULT" "feidename"
      password = iniFile >>= lookupValue "DEFAULT" "password"
  case sequence [feidename, password] of
    Right [fn, pwd] -> return (fn, pwd) -- Is tuple needed?
    Left err        -> error err

nameValuePair tag = (fromAttrib "name" tag, fromAttrib "value" tag)

toByteString = encodeUtf8 . toLazyText . htmlEncodedText . L.toStrict . decodeUtf8 . renderTags

--fetchHiddenFields :: Session -> IO [(String, String)]
fetchHiddenFields sess = do
  getRes <- Sess.get sess "https://fsweb.no/studentweb/login.jsf?inst=FSUIB"
  let extraRe = [re|mojarra\.jsfcljs\(.*{'(.*)':'(.*)'}.*\)|]
      tags = map nameValuePair $ filter (~== "<input type=hidden>") feideModuleBox
      feideModuleBox =
        takeWhile (~/= "</section>") . dropWhile (~/= "<section data-flap-name=feide>") . parseTags $
        getRes ^. responseBody
      additional = matches $ toByteString feideModuleBox *=~ extraRe
      fields = tags
  print fields
  print additional

scrap :: IO ()
scrap = do
  sess <- Sess.newSession
  fetchHiddenFields sess
