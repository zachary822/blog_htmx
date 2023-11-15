{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Configuration.Dotenv
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
import Network.URI
import Network.Wai.Handler.Warp
import Network.Wai.Middleware.RequestLogger
import System.Environment
import Text.Blaze.Html.Renderer.Utf8 (renderHtml)
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
mdToHtml s =
  runIOorExplode $
    readMarkdown def{readerExtensions = extensionsFromList [Ext_backtick_code_blocks]} s
      >>= writeHtml5 def

main :: IO ()
main = do
  onMissingFile (loadFile defaultConfig) mempty

  debug <- isDebug

  dburi <- getEnv "MONGODB_URI"
  webHost <- fromMaybe "localhost" <$> lookupEnv "HOST"
  webPort <- fromMaybe "3000" <$> lookupEnv "PORT"

  let (dbhost, uname, passwd) = getDbInfo dburi

  rs <- openReplicaSetSRV' dbhost

  pool <- newPool (defaultPoolConfig (getPipe rs uname passwd) close 10 10)

  let opts =
        defaultOptions
          { verbose = 1
          , settings =
              setHost (fromString webHost) . setPort (read webPort) $
                settings defaultOptions
          }

  scottyOpts opts $ do
    middleware $
      if debug
        then logStdoutDev
        else logStdout

    get "/posts" $ do
      setHeader "content-type" "text/html; charset=utf-8"

      ps <- liftIO $ withResource pool $ \p ->
        access p master "blog" $
          find (select ["published" =: True] "posts"){sort = ["updated" =: (-1 :: Int)]} >>= rest

      posts <-
        liftIO
          ( forM ps $ \p -> do
              title <- M.lookup "title" p
              htmlPost <- M.lookup "body" p >>= mdToHtml

              return $
                H.article H.! A.class_ "prose prose-slate" $ do
                  H.h1 $ H.toHtml (title :: Text)
                  htmlPost
          )

      raw . renderHtml . sequence_ $ intersperse (H.div H.! A.class_ "divider" $ mempty) posts
