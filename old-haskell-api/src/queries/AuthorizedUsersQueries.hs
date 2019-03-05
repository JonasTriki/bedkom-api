{--

{-# LANGUAGE OverloadedStrings #-}

module AuthorizedUsersQueries where

import Database.PostgreSQL.Simple

isUserAuthorized :: Query
isUserAuthorized = "select exists(select 1 from authorized_users where id = '?' and year = '?' and semester = '?')"

authorizeUser :: Query
authorizeUser = "insert into authorized_users (id, year, semester) values (?, ?, ?)"

--}