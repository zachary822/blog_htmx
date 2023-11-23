{-# LANGUAGE OverloadedStrings #-}

module Lib.Database where

import Control.Monad
import Data.Maybe
import Data.Text qualified as T
import Database.MongoDB
import Network.URI

getDbInfo :: String -> (String, Username, Password)
getDbInfo uri = fromJust $ do
  parsed <- parseURI uri

  guard $ uriScheme parsed == "mongodb+srv:"

  uriAuth <- uriAuthority parsed

  let (uname, pw) = T.breakOn ":" . T.pack $ uriUserInfo uriAuth

  return (uriRegName uriAuth, uname, T.tail . T.init $ pw)

getPipe :: ReplicaSet -> Username -> Password -> IO Pipe
getPipe rs uname passwd = do
  pipe <- primary rs
  _ <- access pipe master admin (auth uname passwd)
  return pipe
