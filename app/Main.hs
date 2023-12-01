{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Configuration.Dotenv (defaultConfig, loadFile, onMissingFile)
import Control.Exception (bracket)
import Control.Monad
import Control.Monad.Reader
import Data.Default (def)
import Data.Maybe
import Data.Pool
import Data.String (fromString)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time.Calendar.Month
import Database.MongoDB hiding (Oid, next)
import Database.MongoDB qualified as M
import GHC.IO (unsafePerformIO)
import Katip
import Katip.Monadic (askLoggerIO)
import Lib.Blaze
import Lib.Database
import Lib.Middleware
import Lib.Rss
import Lib.Rss.Attributes qualified as RA
import Lib.Types
import Lib.Utils
import Network.HTTP.Types
import Network.Wai.Handler.Warp
import Network.Wai.Middleware.RequestLogger
import System.Environment
import System.IO
import Text.Blaze.Html5 qualified as H
import Text.Blaze.Html5.Attributes qualified as A
import Web.Scotty.Trans

main :: IO ()
main = do
  onMissingFile (loadFile defaultConfig) mempty

  debug <- isDebug

  dburi <- getEnv "MONGODB_URI"
  webHost <- fromMaybe "*" <$> lookupEnv "HOST"
  webPort <- fromMaybe "3000" <$> lookupEnv "PORT"
  socketPath :: Maybe FilePath <- lookupEnv "SOCKET"

  let (dbhost, uname, passwd) = getDbInfo dburi

  rs <- openReplicaSetSRV' dbhost

  pool <- newPool (defaultPoolConfig (getPipe rs uname passwd) M.close 10 5)

  handleScribe <- mkHandleScribe ColorIfTerminal stdout (permitItem InfoS) V2
  let makeLogEnv =
        registerScribe "stdout" handleScribe defaultScribeSettings
          =<< initLogEnv "MyApp" "production"

  bracket makeLogEnv closeScribes $ \le -> do
    let config =
          AppConfig
            { getPool = pool
            , logEnv = le
            , logContext = mempty
            , logNamespace = "main"
            }
    let f =
          flip
            runReaderT
            config
            . unApp
    let opts =
          defaultOptions
            { settings =
                setHost (fromString webHost) . setPort (read webPort) $
                  settings defaultOptions
            }

    logger <- f askLoggerIO

    let destination = Callback $ logger InfoS . logStr . FLogStr
        logRequest =
          unsafePerformIO $
            mkRequestLogger
              def
                { outputFormat = Apache FromSocket
                , destination = destination
                }
        logRequestDev =
          unsafePerformIO $
            mkRequestLogger
              def
                { destination = destination
                }

    scottySocketT' socketPath opts f $ do
      middleware $
        if debug
          then logRequestDev
          else logRequest

      middleware rewriteHtmxPostsMiddleware

      get "/posts/" $ do
        page <- getPage

        result <-
          head
            <$> runDb
              "blog"
              ( aggregate
                  "posts"
                  ( postsPipeline
                      (pageLimit page)
                      (pageOffset page)
                  )
              )

        let docs = M.at "data" result

        when (null docs) $ raiseStatus status404 "no posts"

        let total = getTotal result
            curr = getCurrentPage page
            maxPage = getMaxPage total page

        blazeHtml $ do
          renderPosts docs
          when (maxPage > 1) $
            H.div H.! [classQQ| join |] $
              forM_ [1 .. maxPage] $ \case
                x
                  | x == curr ->
                      H.button
                        H.! A.class_ "join-item btn btn-active"
                        $ H.toHtml (show curr)
                o ->
                  H.button
                    H.! A.class_ "join-item btn"
                    H.! hx "push-url" "true"
                    H.! hx "get" (fromString $ "/posts/?offset=" <> show (pred o * pageLimit page))
                    H.! hx "target" "#posts"
                    $ fromString (show o)

      get "/posts/:pid" $ do
        Oid pid <- captureParam "pid"

        runDb
          "blog"
          ( find (select (getPost pid) "posts") >>= M.rest
          )
          >>= \case
            [] -> raiseStatus status404 "Not Found"
            doc : _ -> blazeHtml $ do
              postHtml $ fromDocument doc

      get "/posts/summary" $ do
        result <-
          head
            <$> runDb
              "blog"
              ( aggregate
                  "posts"
                  summaryPipeline
              )

        let summary = fromDocument result :: Summary

        blazeHtml $ do
          H.h2 "Months"
          H.ul $
            forM_ (monthly summary) $ \m -> do
              H.li $ do
                let month@(YearMonth y my) = summaryMonth m

                H.a
                  H.! A.href "#"
                  H.! hx "push-url" "true"
                  H.! hx "target" "#posts"
                  H.! hx "get" (fromString $ concat ["/posts/months/", show y, "/", show my])
                  $ do
                    fromString . show $ month
                    " ("
                    fromString . show $ monthCount m
                    ")"
          H.h2 "Tags"
          H.ul $
            forM_ (tags summary) $ \t -> do
              H.li $ do
                H.a
                  H.! A.href "#"
                  H.! hx "push-url" "true"
                  H.! hx "target" "#posts"
                  H.! hx "get" (fromString . T.unpack $ "/posts/tags/" <> tagName t)
                  $ do
                    H.toHtml $ tagName t
                    " ("
                    fromString . show $ tagCount t
                    ")"

      get "/posts/tags/:tag" $ do
        tag :: Text <- captureParam "tag"
        when (T.null tag) $ raiseStatus status404 "empty tag"

        page <- getPage

        result <-
          head
            <$> runDb
              "blog"
              ( aggregate
                  "posts"
                  ( postsTagPipline
                      tag
                      (pageLimit page)
                      (pageOffset page)
                  )
              )
        let docs = M.at "data" result

        when (null docs) $ raiseStatus status404 "no posts"

        let total = getTotal result
            curr = getCurrentPage page
            maxPage = getMaxPage total page

        blazeHtml $ do
          renderPosts docs
          when (maxPage > 1) $
            H.div H.! [classQQ| join |] $
              forM_ [1 .. maxPage] $ \case
                x
                  | x == curr ->
                      H.button
                        H.! A.class_ "join-item btn btn-active"
                        $ H.toHtml (show curr)
                o ->
                  H.button
                    H.! A.class_ "join-item btn"
                    H.! hx
                      "get"
                      ( fromString . T.unpack $
                          T.concat
                            [ "/posts/tags/"
                            , tag
                            , "?offset="
                            , T.pack $ show (pred o * pageLimit page)
                            ]
                      )
                    H.! hx "push-url" "true"
                    H.! hx "target" "#posts"
                    $ fromString (show o)

      get "/posts/months/:year/:month" $ do
        year :: Int <- captureParam "year"
        month :: Int <- captureParam "month"
        page <- getPage

        result <-
          head
            <$> runDb
              "blog"
              ( aggregate
                  "posts"
                  ( postsMonthPipline
                      year
                      month
                      (pageLimit page)
                      (pageOffset page)
                  )
              )

        let docs = M.at "data" result

        when (null docs) $ raiseStatus status404 "no posts"

        let total = getTotal result
            curr = getCurrentPage page
            maxPage = getMaxPage total page

        blazeHtml $ do
          renderPosts docs
          when (maxPage > 1) $
            H.div H.! [classQQ| join |] $
              forM_ [1 .. maxPage] $ \case
                x
                  | x == curr ->
                      H.button
                        H.! A.class_ "join-item btn btn-active"
                        $ H.toHtml (show curr)
                o ->
                  H.button
                    H.! A.class_ "join-item btn"
                    H.! hx
                      "get"
                      ( fromString . T.unpack $
                          T.concat
                            [ "/posts/months/"
                            , T.pack $ show year
                            , "/"
                            , T.pack $ show month
                            , "?offset="
                            , T.pack $ show (pred o * pageLimit page)
                            ]
                      )
                    H.! hx "push-url" "true"
                    H.! hx "target" "#posts"
                    $ fromString (show o)

      get "/posts/search" $ do
        page <- getPage
        q <- T.strip <$> queryParam "q"
        when (T.null q) $ redirect "/posts/"

        docs <-
          runDb
            "blog"
            ( aggregate
                "posts"
                ( postsSearchPipeline
                    q
                    (pageLimit page)
                    (pageOffset page)
                )
            )
        blazeHtml $ do
          if null docs
            then do
              H.div "No Results Found"
            else do
              renderPosts docs

      get "/posts/feed" $ do
        posts :: [Post] <-
          map fromDocument
            <$> runDb
              "blog"
              ( aggregate
                  "posts"
                  ( postsPipeline'
                      (10 :: Int)
                      (0 :: Int)
                  )
              )

        rssXml $ do
          xmlHeader
          rss . channel $ do
            title "ThoughtBank Blog"
            link "http://localhost:5173"
            atomLink RA.! RA.href "http://localhost:5173/posts/feed" RA.! RA.rel "self"
            description "Thoughts and concerns from my programming journey."
            language "en-us"
            forM_ posts $ \p -> item $ do
              let l = toRss $ "http://localhost:5173/posts/" <> show (postId p)

              title . toRss $ postTitle p
              pubDate . toRss . formatRfc822Gmt $ postUpdated p
              link l
              guid l
              description . cdata . mdToHtml $ postBody p
