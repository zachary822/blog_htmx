{-# LANGUAGE OverloadedStrings #-}

module Lib.Database where

import Control.Monad
import Data.Maybe
import Data.Text (Text)
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

-- pipelines
postsPipline :: (Val v0, Val v1) => v0 -> v1 -> [Document]
postsPipline limit offset =
  [
    [ "$facet"
        =: [ "data"
              =: [ ["$match" =: ["published" =: True]]
                 , ["$sort" =: ["updated" =: (-1 :: Int)]]
                 , ["$limit" =: limit]
                 , ["$skip" =: offset]
                 , ["$project" =: ["published" =: (0 :: Int)]]
                 ]
           , "total"
              =: [ ["$match" =: ["published" =: True]]
                 , ["$count" =: ("total" :: Text)]
                 ]
           ]
    ]
  ,
    [ "$project"
        =: [ "data" =: (1 :: Int)
           , "total" =: ["$first" =: ("$total.total" :: Text)]
           ]
    ]
  ]

postsTagPipline :: (Val v0, Val v1) => Text -> v0 -> v1 -> [Document]
postsTagPipline tag limit offset =
  [
    [ "$facet"
        =: [ "data"
              =: [
                   [ "$match"
                      =: [ "published" =: True
                         , "tags" =: ["$in" =: [tag]]
                         ]
                   ]
                 , ["$sort" =: ["updated" =: (-1 :: Int)]]
                 , ["$limit" =: limit]
                 , ["$skip" =: offset]
                 , ["$project" =: ["published" =: (0 :: Int)]]
                 ]
           , "total"
              =: [
                   [ "$match"
                      =: [ "published" =: True
                         , "tags" =: ["$in" =: [tag]]
                         ]
                   ]
                 , ["$count" =: ("total" :: Text)]
                 ]
           ]
    ]
  ,
    [ "$project"
        =: [ "data" =: (1 :: Int)
           , "total" =: ["$first" =: ("$total.total" :: Text)]
           ]
    ]
  ]

summaryPipeline :: [Document]
summaryPipeline =
  [
    [ "$facet"
        =: [ "monthly"
              =: [ ["$match" =: ["published" =: True]]
                 ,
                   [ "$group"
                      =: [ "_id"
                            =: [ "year" =: ["$year" =: ("$created" :: Text)]
                               , "month" =: ["$month" =: ("$created" :: Text)]
                               ]
                         , "count" =: ["$sum" =: (1 :: Int)]
                         ]
                   ]
                 ,
                   [ "$project"
                      =: [ "_id" =: (0 :: Int)
                         , "year" =: ("$_id.year" :: Text)
                         , "month" =: ("$_id.month" :: Text)
                         , "count" =: (1 :: Int)
                         ]
                   ]
                 ]
           , "tags"
              =: [ ["$match" =: ["published" =: True]]
                 , ["$unwind" =: ["path" =: ("$tags" :: Text)]]
                 ,
                   [ "$group"
                      =: [ "_id" =: ("$tags" :: Text)
                         , "count" =: ["$sum" =: (1 :: Int)]
                         ]
                   ]
                 ,
                   [ "$project"
                      =: [ "_id" =: (0 :: Int)
                         , "name" =: ("$_id" :: Text)
                         , "count" =: (1 :: Int)
                         ]
                   ]
                 , ["$sort" =: ["count" =: (-1 :: Int), "name" =: (-1 :: Int)]]
                 ]
           ]
    ]
  ]