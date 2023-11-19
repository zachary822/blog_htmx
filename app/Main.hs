{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Configuration.Dotenv
import Control.Exception
import Control.Monad
import Control.Monad.IO.Class
import Data.Functor ((<&>))
import Data.List (intersperse)
import Data.Maybe
import Data.Pool
import Data.String (fromString)
import Data.Text (Text)
import Data.Text qualified as T
import Database.MongoDB
import Database.MongoDB qualified as M
import GHC.Generics
import Lib.Blaze
import Network.Socket as S
import Network.URI
import Network.Wai.Handler.Warp
import Network.Wai.Middleware.RequestLogger
import System.Directory
import System.Environment
import Text.Blaze.Html5 (Html)
import Text.Blaze.Html5 qualified as H
import Text.Blaze.Html5.Attributes qualified as A
import Text.Pandoc (
  Extension (..),
  ReaderOptions (..),
  def,
  extensionsFromList,
  readMarkdown,
  runIOorExplode,
  writeHtml5,
 )
import Web.Scotty

isDebug :: IO Bool
isDebug =
  lookupEnv "DEBUG" <&> \case
    Just "1" -> True
    _ -> False

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

mdToHtml :: Text -> IO Html
mdToHtml =
  runIOorExplode
    . ( writeHtml5 def
          <=< readMarkdown def{readerExtensions = extensionsFromList [Ext_backtick_code_blocks]}
      )

scottySocket' :: Maybe String -> Options -> ScottyM () -> IO ()
scottySocket' mpath opts app = case mpath of
  Nothing -> do
    scottyOpts opts app
  Just p -> do
    let cleanup = (>> removeFile p) . S.close
    bracket (socket AF_UNIX Stream 0) cleanup $ \sock -> do
      bind sock $ SockAddrUnix p
      listen sock maxListenQueue
      scottySocket opts sock app

data Post = Post
  { postId :: ObjectId
  , postTitle :: Text
  , postBody :: Text
  }
  deriving (Generic, Show, Eq)

class FromDocument a where
  fromDocument :: Document -> a

instance FromDocument Document where
  fromDocument = id

instance FromDocument Post where
  fromDocument d = Post{..}
   where
    postId = M.at "_id" d
    postTitle = M.at "title" d
    postBody = M.at "body" d

postHtml :: Post -> Html -> Html
postHtml p pb = H.article
  H.! [classQQ|
              prose
              prose-slate
              prose-a:no-underline
              prose-a:bg-gradient-to-r
              prose-a:from-indigo-500
              prose-a:via-purple-500
              prose-a:to-pink-500
              prose-a:bg-left-bottom
              prose-a:bg-no-repeat
              prose-a:bg-underline
              hover:prose-a:bg-underline-hover
              prose-a:transition-background-size
              prose-a:font-black
              |]
  $ do
    H.h1 . H.toHtml $ postTitle p
    pb

main :: IO ()
main = do
  onMissingFile (loadFile defaultConfig) mempty

  debug <- isDebug

  dburi <- getEnv "MONGODB_URI"
  webHost <- fromMaybe "*" <$> lookupEnv "HOST"
  webPort <- fromMaybe "3000" <$> lookupEnv "PORT"
  socketPath <- lookupEnv "SOCKET"

  let (dbhost, uname, passwd) = getDbInfo dburi

  rs <- openReplicaSetSRV' dbhost

  pool <- newPool (defaultPoolConfig (getPipe rs uname passwd) M.close 10 10)

  let opts =
        defaultOptions
          { settings =
              setHost (fromString webHost) . setPort (read webPort) $
                settings defaultOptions
          }

  scottySocket' socketPath opts $ do
    middleware $
      if debug
        then logStdoutDev
        else logStdout

    get "/posts" $ do
      ds <- liftIO $ withResource pool $ \p ->
        access p master "blog" $
          find (select ["published" =: True] "posts"){sort = ["updated" =: (-1 :: Int)]} >>= rest

      posts <-
        liftIO
          ( forM ds $ \d -> do
              let p = fromDocument d
              pb <- mdToHtml $ postBody p
              return $ postHtml p pb
          )

      blazeHtml . sequence_ $
        intersperse
          (H.div H.! A.class_ "divider" $ mempty)
          posts
