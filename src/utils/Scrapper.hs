{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings    #-}

module Scrapper where

import           Control.Lens            hiding (re)
import           Control.Monad           (sequence)
import           Data.Aeson              (toJSON)
import qualified Data.ByteString.Lazy    as BL
import           Data.Ini                hiding (sections)
import           Data.List               (find)
import qualified Data.Map                as Map
import           Data.Maybe              (fromJust)
import qualified Data.Text               as T
import qualified Data.Text.Lazy          as L
import           Data.Text.Lazy.Builder  (toLazyText)
import           Data.Text.Lazy.Encoding
import           HTMLEntities.Decoder
import qualified Network.HTTP.Client     as HTTP
import           Network.Wreq
import qualified Network.Wreq.Session    as Sess
import           Text.HTML.TagSoup
import           Text.Regex.TDFA

readCredentials :: IO (T.Text, T.Text)
readCredentials = do
  iniFile <- readIniFile "me.ini"
  let feidename = iniFile >>= lookupValue "DEFAULT" "feidename"
      password = iniFile >>= lookupValue "DEFAULT" "password"
  case sequence [feidename, password] of
    Right [fn, pwd] -> return (fn, pwd) -- Is tuple needed?
    Left err        -> error err

nameValuePair tag = (fromAttrib "name" tag, fromAttrib "value" tag)

toByteString :: [Tag BL.ByteString] -> BL.ByteString
toByteString = encodeUtf8 . toLazyText . htmlEncodedText . L.toStrict . decodeUtf8 . renderTags

toFormParams :: [(BL.ByteString, BL.ByteString)] -> [FormParam]
toFormParams = map (\(k, v) -> BL.toStrict k := v)

findFirstPairAttrib :: [Tag BL.ByteString] -> (BL.ByteString, BL.ByteString) -> BL.ByteString -> BL.ByteString
findFirstPairAttrib tags pair attrib = fromAttrib attrib . fromJust $ find (~== TagOpen "" [pair]) tags

fetchStudy :: Sess.Session -> IO BL.ByteString
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
  loginPostHistoryResp <-
    Sess.customHistoriedPayloadMethodWith "POST" defaults sess "https://fsweb.no/studentweb/login.jsf" fields
  let finalReqUrl = show . HTTP.getUri $ loginPostHistoryResp ^. hrFinalRequest
      tags = parseTags $ loginPostHistoryResp ^. (hrFinalResponse . responseBody)
      asLen = findFirstPairAttrib tags ("name", "asLen") "value"
      authState = findFirstPairAttrib tags ("name", "AuthState") "value"
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
  -- And send the POST request with the payload above
  loginPostRes <- Sess.post sess finalReqUrl loginPostPayload
  let tags = parseTags $ loginPostRes ^. responseBody
      finalPostPayload = ["SAMLResponse" := findFirstPairAttrib tags ("name", "SAMLResponse") "value"]
  -- Finally, we sign in to StudentWeb
  studentWeb <- Sess.post sess "https://fsweb.no/studentweb/samlsso.jsf" finalPostPayload
  currentStudies <- Sess.get sess "https://fsweb.no/studentweb/studier.jsf"
  let tags = parseTags $ currentStudies ^. responseBody
      studyTitle = fromTagText . fromJust $ find isTagText $ dropWhile (~/= "<span class=studieTittel>") tags
  return studyTitle

scrap :: IO ()
scrap = do
  sess <- Sess.newSession
  fetchStudy sess >>= print
