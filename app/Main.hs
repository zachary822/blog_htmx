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
import Data.Either
import Data.Functor ((<&>))
import Data.List (intersperse)
import Data.Maybe
import Data.Pool
import Data.String (fromString)
import Data.Text (Text)
import Data.Time.Clock
import Data.Time.Format
import Data.Time.Format.ISO8601
import Data.Word
import Database.MongoDB
import Database.MongoDB qualified as M
import GHC.Generics
import Lib.Blaze
import Lib.Database
import Network.Socket as S
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
  runPure,
  writeHtml5,
 )
import Web.Scotty

isDebug :: IO Bool
isDebug =
  lookupEnv "DEBUG" <&> \case
    Just "1" -> True
    _ -> False

mdToHtml :: Text -> Html
mdToHtml =
  fromRight mempty
    . runPure
    . ( writeHtml5 def
          <=< readMarkdown def{readerExtensions = extensionsFromList [Ext_backtick_code_blocks]}
      )

scottySocket' :: Maybe FilePath -> Options -> ScottyM () -> IO ()
scottySocket' mpath opts app = case mpath of
  Nothing -> do
    scottyOpts opts app
  Just p -> do
    let cleanup s = do
          S.close s
          removeFile p
    bracket (socket AF_UNIX Stream 0) cleanup $ \sock -> do
      bind sock $ SockAddrUnix p
      listen sock maxListenQueue
      scottySocket opts sock app

data Page = Page
  { pageLimit :: Word32
  , pageOffset :: Word32
  }
  deriving (Show, Eq)

returnDefault :: a -> StatusError -> ActionM a
returnDefault a = const (return a)

getPage :: ActionM Page
getPage = do
  pageLimit <- queryParam "limit" `rescue` returnDefault 10
  pageOffset <- queryParam "offset" `rescue` returnDefault 0

  return Page{..}

data Post = Post
  { postId :: ObjectId
  , postTitle :: Text
  , postBody :: Text
  , postCreated :: UTCTime
  , postUpdated :: UTCTime
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
    postCreated = M.at "created" d
    postUpdated = M.at "updated" d

timeEl :: (FormatTime t, ISO8601 t) => t -> Html
timeEl t = do
  H.time H.! A.datetime (fromString $ formatShow iso8601Format t) $
    H.toHtml $
      formatTime defaultTimeLocale "%c" t

postHtml :: Post -> Html
postHtml p = H.article
  H.! [classQQ|
              prose
              prose-slate
              prose-a:bg-gradient-to-r
              prose-a:from-indigo-500
              prose-a:via-purple-500
              prose-a:to-pink-500
              prose-a:bg-left-bottom
              prose-a:bg-no-repeat
              prose-a:bg-underline
              hover:prose-a:bg-underline-hover
              hover:prose-a:no-underline
              prose-a:transition-background-size
              prose-h1:mb-1
              |]
  $ do
    H.header $ do
      H.h1 $ H.toHtml (postTitle p)
      H.p H.! [classQQ| m-0 text-sm |] $ do
        H.span H.! [classQQ| font-bold mx-1 |] $ "Created:"
        timeEl (postCreated p)
        H.span H.! [classQQ| font-bold mx-1 |] $ "Updated:"
        timeEl (postUpdated p)
    mdToHtml $ postBody p

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
      page <- getPage

      docs <- liftIO . withResource pool $ \p -> do
        access p slaveOk "blog" $
          find
            (select ["published" =: True] "posts")
              { sort = ["updated" =: (-1 :: Int)]
              , limit = pageLimit page
              , skip = pageOffset page
              }
            >>= rest

      let posts = map (postHtml . fromDocument) docs

      blazeHtml $ do
        sequence_ $
          intersperse
            (H.div H.! A.class_ "divider" $ mempty)
            posts
