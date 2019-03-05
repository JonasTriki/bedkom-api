{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

module Schema where

import           Control.Monad.IO.Class (liftIO)
import           Data.Text              (Text)
import           Database.Persist
import           Database.Persist.TH
import           Semester               (Semester)
import           StudyProgram           (StudyProgram)
import           UserRole               (UserRole)

share
  [mkPersist sqlSettings, mkDeleteCascade sqlSettings, mkMigrate "migrateAll"]
  [persistLowerCase|
  User
    firstName Text
    lastName Text
    email Text
    studyProgram StudyProgram
    year Int
    role UserRole
    hash Text
    deriving Eq Show
  LastAuthorized
    userId UserId
    year Int
    semester Semester
    deriving Eq Show
  Dot
    userId UserId
    year Int
    semester Semester
    dots Int
    deriving Eq Show
  Company
    name Text
    description Text
    website Text
    bannerUrl Text Maybe
    deriving Eq Show
  Menu
    name Text
    deriving Eq Show
  FoodEntry
    name Text
    details Text
    menuId MenuId
    deriving Eq Show
  Presentation
    semester Semester
    year Int
    capacity Int
    startTime Int
    endTime Int
    companyId CompanyId
    menuId MenuId Maybe
    deriving Eq Show
  Organizer
    presentationId PresentationId
    userId UserId
  Registration
    presentationId PresentationId
    userId UserId
  Waitlist
    presentationId PresentationId
    userId UserId
|]
