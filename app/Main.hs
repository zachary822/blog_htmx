{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Configuration.Dotenv (defaultConfig, loadFile, onMissingFile)
import Control.Exception
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader
import Control.Retry
import Data.Either
import Data.Functor ((<&>))
import Data.List (intersperse)
import Data.Maybe
import Data.Pool
import Data.String (fromString)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time.Calendar.Month
import Data.Time.Format
import Data.Time.Format.ISO8601
import Database.MongoDB
import Database.MongoDB qualified as M
import Lib.Blaze
import Lib.Database
import Lib.Middleware
import Lib.Rss
import Lib.Types
import Network.HTTP.Types
import Network.Socket as S
import Network.Wai (Response)
import Network.Wai.Handler.Warp
import Network.Wai.Middleware.RequestLogger
import System.Directory
import System.Environment
import System.Posix.Files
import Text.Blaze.Html5 (Html)
import Text.Blaze.Html5 qualified as H
import Text.Blaze.Html5.Attributes qualified as A
import Text.Pandoc (
  Extension (..),
  Inline (..),
  ReaderOptions (..),
  WriterOptions (..),
  def,
  extensionsFromList,
  readMarkdown,
  runPure,
  writeHtml5,
 )
import Text.Pandoc.Walk
import Web.Scotty.Trans

isDebug :: IO Bool
isDebug =
  lookupEnv "DEBUG" <&> \case
    Just "1" -> True
    _ -> False

isHxRequest :: ActionM Bool
isHxRequest =
  header "hx-request" <&> \case
    Just "true" -> True
    _ -> False

formatLink :: (Monad m) => Inline -> m Inline
formatLink (Link (id_, cls, attrs) content target) = do
  return $ Link (id_, cls, ("rel", "noopener noreferrer") : ("target", "_blank") : attrs) content target
formatLink a = return a

mdToHtml :: Text -> Html
mdToHtml =
  fromRight mempty
    . runPure
    . ( writeHtml5 def{writerExtensions = extensionsFromList [Ext_raw_html]}
          <=< walkM formatLink
          <=< readMarkdown def{readerExtensions = extensionsFromList [Ext_backtick_code_blocks, Ext_raw_html]}
      )

scottySocketT' :: Maybe FilePath -> Options -> (ConfigReader Response -> IO Response) -> ScottyM () -> IO ()
scottySocketT' mpath opts pool app = case mpath of
  Nothing -> do
    scottyOptsT opts pool app
  Just p -> do
    let cleanup s = do
          S.close s
          removeFile p
    bracket (socket AF_UNIX Stream 0) cleanup $ \sock -> do
      bind sock $ SockAddrUnix p
      setFileMode p 0o777
      listen sock maxListenQueue
      scottySocketT opts sock pool app

returnDefault :: a -> StatusError -> ActionM a
returnDefault a = const (return a)

getPage :: ActionM Page
getPage = do
  pageLimit <- queryParam "limit" `rescue` returnDefault 10
  pageOffset <- queryParam "offset" `rescue` returnDefault 0

  return Page{..}

timeEl :: (FormatTime t, ISO8601 t) => t -> Html
timeEl t = do
  H.time H.! A.datetime (fromString $ formatShow iso8601Format t) $
    H.toHtml $
      formatTime defaultTimeLocale "%c" t

{-
The fun underline classes
              prose-a:bg-gradient-to-r
              prose-a:from-indigo-500
              prose-a:via-purple-500
              prose-a:to-pink-500
              prose-a:bg-left-bottom
              prose-a:bg-no-repeat
              prose-a:bg-underline
              hover:prose-a:bg-underline-hover
              prose-a:transition-background-size
-}

postHtml :: Post -> Html
postHtml p = H.article
  H.! [classQQ|
              prose
              prose-h1:mb-1
              prose-h1:no-underline
              |]
  $ do
    H.header $ do
      let pid = show $ postId p
      H.a
        H.! A.id (fromString $ "posts:" <> pid)
        H.! hx "get" (fromString $ "/posts/" <> pid)
        H.! hx "push-url" "true"
        H.! hx "target" "#posts"
        $ do
          H.h1 $ H.toHtml (postTitle p)
      H.div H.! [classQQ| mt-0 mb-1 text-sm |] $ do
        "Created: "
        timeEl (postCreated p)
        " (Updated: "
        timeEl (postUpdated p)
        ")"
      H.div H.! [classQQ| my-0 flex gap-2 |] $ do
        forM_ (postTags p) $ \t -> do
          H.button
            H.! [classQQ| badge badge-neutral |]
            H.! hx "target" "#posts"
            H.! hx "get" (fromString . T.unpack $ "/posts/tags/" <> t)
            $ H.toHtml t
    mdToHtml $ postBody p

divider :: Html
divider = H.div H.! A.class_ "divider" $ mempty

renderPosts :: [Document] -> Html
renderPosts =
  sequence_
    . intersperse divider
    . map (postHtml . fromDocument)

getTotal :: Document -> Integer
getTotal = M.at "total"

getCurrentPage :: Page -> Integer
getCurrentPage page = pageOffset page `div` pageLimit page + 1

getMaxPage :: (Integral a, Integral b) => a -> Page -> b
getMaxPage total page = ceiling (fromIntegral total / fromIntegral (pageLimit page) :: Double)

runDb :: (MonadIO m) => Database -> Action IO b -> ActionT (ReaderT Config m) b
runDb dbname q = do
  pool <- getPool <$> lift ask
  liftAndCatchIO $
    recoverAll limitedBackoff $
      const $ do
        withResource pool $ \p ->
          access p master dbname q

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

  let f = flip runReaderT Config{getPool = pool}
      opts =
        defaultOptions
          { settings =
              setHost (fromString webHost) . setPort (read webPort) $
                settings defaultOptions
          }

  scottySocketT' socketPath opts f $ do
    middleware $
      if debug
        then logStdoutDev
        else logStdout

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
      pid :: ObjectId <- captureParam "pid"

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
      if null docs
        then blazeHtml $ do
          H.div "No Results Found"
        else blazeHtml $ do
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
        rss $ do
          title "ThoughtBank Blog"
          link "http://localhost:5173"
          description "Thoughts and concerns from my programming journey."
          language "en-us"
          forM_ posts $ \p -> item $ do
            let l = toRss $ "http://localhost:5173/posts/" <> show (postId p)

            title . toRss $ postTitle p
            pubDate . toRss . formatTime defaultTimeLocale "%a, %e %b %Y %R %z" $ postUpdated p
            link l
            guid l
            description . cdata . mdToHtml $ postBody p
