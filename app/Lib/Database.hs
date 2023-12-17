{-# LANGUAGE OverloadedStrings #-}

module Lib.Database where

import Control.Exception (SomeException)
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Reader
import Control.Retry
import Data.Bifunctor
import Data.Maybe
import Data.Pool
import Data.Text (Text)
import Data.Text qualified as T
import Database.MongoDB
import Katip
import Katip.Monadic (askLoggerIO)
import Lib.Types
import Network.URI
import Web.Scotty.Trans

getDbInfo :: String -> (String, Username, Password)
getDbInfo uri = fromJust $ do
  parsed <- parseURI uri

  guard $ uriScheme parsed == "mongodb+srv:"

  uriAuth <- uriAuthority parsed

  let (uname, pw) = second (T.tail . T.init) $ T.breakOn ":" . T.pack $ uriUserInfo uriAuth

  return (uriRegName uriAuth, uname, pw)

getPipe :: ReplicaSet -> Username -> Password -> IO Pipe
getPipe rs uname passwd = do
  pipe <- primary rs
  _ <- access pipe master admin (auth uname passwd)
  return pipe

runDb :: (MonadIO m) => Database -> Action IO b -> ActionT (App m) b
runDb dbname q = do
  pool <- lift $ asks getPool

  logger <- lift $ katipAddNamespace "runDb" askLoggerIO

  liftIO
    $ recovering
      limitedBackoff
      [ logRetries
          (\(_ :: SomeException) -> return True)
          (\a b c -> logger WarningS $ logStr (defaultLogMsg a b c))
      ]
    $ const
    $ do
      withResource pool $ \p ->
        access p master dbname q

-- pipelines
postsPipeline' :: (Val v0, Val v1) => v0 -> v1 -> [Document]
postsPipeline' limit offset =
  [ ["$match" =: ["published" =: True]]
  , ["$sort" =: ["updated" =: (-1 :: Int)]]
  , ["$limit" =: limit]
  , ["$skip" =: offset]
  , ["$project" =: ["published" =: (0 :: Int)]]
  ]

postsPipeline :: (Val v0, Val v1) => v0 -> v1 -> [Document]
postsPipeline limit offset =
  [
    [ "$facet"
        =: [ "data" =: postsPipeline' limit offset
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

getPost :: ObjectId -> Document
getPost oid =
  [ "_id" =: oid
  , "published" =: True
  ]

postsSearchPipeline :: (Val v0, Val v1) => Text -> v0 -> v1 -> [Document]
postsSearchPipeline q limit offset =
  [
    [ "$search"
        =: [ "text"
              =: [ "query" =: q
                 , "path" =: (["title", "body"] :: [Text])
                 , "fuzzy" =: ([] :: Document)
                 ]
           ]
    ]
  , ["$match" =: ["published" =: True]]
  , ["$limit" =: limit]
  , ["$skip" =: offset]
  , ["$project" =: ["published" =: (0 :: Int)]]
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

postsMonthPipline :: (Val v0, Val v1, Val v2, Val v3) => v0 -> v1 -> v2 -> v3 -> [Document]
postsMonthPipline year month limit offset =
  [
    [ "$facet"
        =: [ "data"
              =: [
                   [ "$match"
                      =: [ "published" =: True
                         ]
                   ]
                 ,
                   [ "$set"
                      =: [ "year" =: ["$year" =: ("$updated" :: Text)]
                         , "month" =: ["$month" =: ("$updated" :: Text)]
                         ]
                   ]
                 , ["$match" =: ["year" =: year, "month" =: month]]
                 , ["$sort" =: ["updated" =: (-1 :: Int)]]
                 , ["$limit" =: limit]
                 , ["$skip" =: offset]
                 ,
                   [ "$project"
                      =: [ "published" =: (0 :: Int)
                         , "year" =: (0 :: Int)
                         , "month" =: (0 :: Int)
                         ]
                   ]
                 ]
           , "total"
              =: [
                   [ "$match"
                      =: [ "published" =: True
                         ]
                   ]
                 ,
                   [ "$set"
                      =: [ "year" =: ["$year" =: ("$updated" :: Text)]
                         , "month" =: ["$month" =: ("$updated" :: Text)]
                         ]
                   ]
                 , ["$match" =: ["year" =: year, "month" =: month]]
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
                            =: [ "year" =: ["$year" =: ("$updated" :: Text)]
                               , "month" =: ["$month" =: ("$updated" :: Text)]
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
                 , ["$sort" =: ["year" =: (-1 :: Int), "month" =: (-1 :: Int), "count" =: (-1 :: Int)]]
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
