{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Configuration.Dotenv (defaultConfig, loadFile, onMissingFile)
import Control.Exception (bracket)
import Control.Monad
import Control.Monad.Reader
import Data.Default (def)
import Data.Pool
import Data.String (fromString)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Lazy qualified as TL
import Data.Time.Calendar.Month
import Database.MongoDB hiding (Oid, next)
import Database.MongoDB qualified as M
import Katip
import Katip.Monadic (askLoggerIO)
import Lib.Blaze
import Lib.Components
import Lib.Database
import Lib.Middleware
import Lib.Rss
import Lib.Rss.Attributes qualified as RA
import Lib.Types
import Lib.Utils
import Network.HTTP.Types
import Network.Wai.Handler.Warp
import Network.Wai.Middleware.RequestLogger
import Network.Wai.Middleware.Rewrite
import System.Environment
import System.IO
import Text.Blaze.Html5 qualified as H
import Text.Blaze.Html5.Attributes qualified as A
import Text.Pandoc.Walk
import Web.Scotty.Trans

main :: IO ()
main = do
  onMissingFile (loadFile defaultConfig) mempty

  debug <- isDebug

  dburi <- getEnv "MONGODB_URI"
  webHost <- maybe "*" fromString <$> lookupEnv "HOST"
  webPort <- maybe 3000 read <$> lookupEnv "PORT"
  socketPath :: Maybe FilePath <- lookupEnv "SOCKET"

  -- logging
  sev :: Severity <- maybe InfoS read <$> lookupEnv "SEVERITY"
  logFile :: Maybe FilePath <- lookupEnv "LOG_FILE"

  let (dbhost, uname, passwd) = getDbInfo dburi

  rs <- openReplicaSetSRV' dbhost

  pool <- newPool (defaultPoolConfig (getPipe rs uname passwd) M.close 10 5)

  stdoutScribe <- mkHandleScribe ColorIfTerminal stdout (permitItem sev) V2
  fileScribe <- maybe mempty (\p -> mkFileScribe p (permitItem sev) V2) logFile

  let makeLogEnv =
        initLogEnv "MyApp" "production"
          >>= registerScribe "stdout" stdoutScribe defaultScribeSettings
          >>= registerScribe "logFile" fileScribe defaultScribeSettings

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
                setHost webHost . setPort webPort $
                  settings defaultOptions
            }

    logger <- f askLoggerIO

    let destination = Callback $ logger InfoS . logStr . FLogStr

    logRequest <-
      mkRequestLogger
        def
          { outputFormat = Apache FromSocket
          , destination = destination
          }
    logRequestDev <-
      mkRequestLogger
        def
          { destination = destination
          }

    scottySocketT' socketPath opts f $ do
      middleware $
        if debug
          then logRequestDev
          else logRequest

      middleware $ rewriteWithQueries rewriteHtmxPosts

      get "/healthcheck" $ do
        ver <- runDb "blog" serverVersion
        text $ TL.fromStrict ver

      get "/posts/" $ do
        page <- getPage

        result <-
          join
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

        when (null docs) $ do
          status status404
          text "no posts"
          finish

        let total = getTotal result
            curr = getCurrentPage page
            maxPage = getMaxPage total page

        blazeHtml $ do
          renderPosts docs
          when (maxPage > 1) $
            H.div H.! A.class_ "join" $
              forM_ [1 .. maxPage] $ \o ->
                pageButton
                  (o == curr)
                  ( T.concat
                      [ "/posts/"
                      , "?offset="
                      , T.pack $ show (pred o * pageLimit page)
                      , "&limit="
                      , T.pack $ show (pageLimit page)
                      ]
                  )
                  o

      get "/posts/:pid" $ do
        Oid pid <- captureParam "pid"

        runDb
          "blog"
          (find (select (getPost pid) "posts") >>= M.rest)
          >>= \case
            [] -> do
              status status404
              text "no posts"
              finish
            doc : _ -> blazeHtml $ do
              postHtml $ fromDocument doc

      get "/posts/summary" $ do
        result <-
          join
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
                    monthUrl = fromString $ concat ["/posts/months/", show y, "/", show my]

                H.a
                  H.! A.href monthUrl
                  H.! hx Get monthUrl
                  H.! hx PushUrl "true"
                  H.! hx Target "#posts"
                  $ do
                    fromString . show $ month
                    " ("
                    fromString . show $ monthCount m
                    ")"
          H.h2 "Tags"
          H.ul $
            forM_ (tags summary) $ \t -> do
              let tagUrl = fromString . T.unpack $ "/posts/tags/" <> tagName t
              H.li $ do
                H.a
                  H.! A.href tagUrl
                  H.! hx PushUrl "true"
                  H.! hx Target "#posts"
                  H.! hx Get tagUrl
                  $ do
                    H.toHtml $ tagName t
                    " ("
                    fromString . show $ tagCount t
                    ")"

      get "/posts/tags/:tag" $ do
        tag :: Text <- captureParam "tag"
        when (T.null tag) $ do
          status status404
          text "empty"
          finish

        page <- getPage

        result <-
          join
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

        when (null docs) $ do
          status status404
          text "no posts"
          finish

        let total = getTotal result
            curr = getCurrentPage page
            maxPage = getMaxPage total page

        blazeHtml $ do
          renderPosts docs
          when (maxPage > 1) $
            H.div H.! A.class_ "join" $
              forM_ [1 .. maxPage] $ \o ->
                pageButton
                  (o == curr)
                  ( T.concat
                      [ "/posts/tags/"
                      , tag
                      , "?offset="
                      , T.pack $ show (pred o * pageLimit page)
                      , "&limit="
                      , T.pack $ show (pageLimit page)
                      ]
                  )
                  o

      get "/posts/months/:year/:month" $ do
        year :: Int <- captureParam "year"
        month :: Int <- captureParam "month"
        page <- getPage

        result <-
          join
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

        when (null docs) $ do
          status status404
          text "no posts"
          finish

        let total = getTotal result
            curr = getCurrentPage page
            maxPage = getMaxPage total page

        blazeHtml $ do
          renderPosts docs
          when (maxPage > 1) $
            H.div H.! A.class_ "join" $
              forM_ [1 .. maxPage] $ \o ->
                pageButton
                  (o == curr)
                  ( T.concat
                      [ "/posts/months/"
                      , T.pack $ show year
                      , "/"
                      , T.pack $ show month
                      , "?offset="
                      , (T.pack . show) (pred o * pageLimit page)
                      , "&limit="
                      , (T.pack . show) (pageLimit page)
                      ]
                  )
                  o

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
              description . cdata . mdToHtml (walkM formatLink) $ postBody p
