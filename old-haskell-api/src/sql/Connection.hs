{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Connection where

import           Schema

import           Control.Lens.TH             (makeLenses)
import           Control.Monad.Logger        (LoggingT, MonadLogger,
                                              runStdoutLoggingT)
import           Control.Monad.Reader        (runReaderT)
import           Data.Either                 (fromRight)
import           Data.Ini.Config.Bidir       (IniSpec, field, ini, number,
                                              parseIni, section, text, (.=), getIniValue)
import           Data.Text                   (Text, pack)
import qualified Data.Text.IO                as T
import           Database.Persist.Postgresql (ConnectionString, SqlPersistT,
                                              runMigration, withPostgresqlConn)
import Control.Lens ((^.))

data ConnectionConfig = ConnectionConfig
  { _ccHost     :: Text
  , _ccPort     :: Int
  , _ccUsername :: Text
  , _ccDbName   :: Text
  , _ccPassword :: Text
  }

makeLenses ''ConnectionConfig

ccLocalhost :: ConnectionConfig
ccLocalhost = ConnectionConfig "localhost" 1337 "admin" "testdb" "123456"

configParser :: IniSpec ConnectionConfig ()
configParser =
  section "DB" $ do
    ccHost .= field "host" text
    ccPort .= field "port" number
    ccUsername .= field "username" text
    ccDbName .= field "dbname" text
    ccPassword .= field "password" text

runAction :: SqlPersistT (LoggingT IO) a -> ConnectionString -> IO a
runAction action connectionString = runStdoutLoggingT . withPostgresqlConn connectionString $ runReaderT action

connString :: IO ConnectionString
connString = do
  file <- T.readFile "config.ini"
  let config = parseIni file $ ini ccLocalhost configParser
  case config of
    Left msg -> error msg
    Right ini -> do
      let conconf = getIniValue ini
      return
        "host=" ++ (conconf ^. ccHost) ++ " port=5432 user=bedkom_admin dbname=postgres password=zLydJbRA4s8nv9hqGdF4ja63"

migrateDB :: IO ()
migrateDB = connString >>= runAction (runMigration migrateAll)
