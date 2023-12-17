{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}

module Lib.Utils where

import Control.Exception (bracket)
import Control.Monad
import Control.Monad.IO.Class
import Data.Either
import Data.Functor ((<&>))
import Data.List (intersperse)
import Data.String (fromString)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time.Clock
import Data.Time.Format
import Data.Time.Format.ISO8601
import Database.MongoDB hiding (Oid, next)
import Database.MongoDB qualified as M
import Lib.Blaze
import Lib.Types
import Network.Socket as S
import Network.Wai (Response)
import System.Directory
import System.Environment
import System.Posix.Files
import Text.Blaze.Html5 (Html)
import Text.Blaze.Html5 qualified as H
import Text.Blaze.Html5.Attributes qualified as A
import Text.Pandoc (
  Extension (..),
  HTMLMathMethod (..),
  Inline (..),
  PandocPure,
  ReaderOptions (..),
  WriterOptions (..),
  def,
  extensionsFromList,
  readMarkdown,
  runPure,
  writeHtml5,
 )
import Text.Pandoc.Definition (Pandoc)
import Text.Pandoc.Walk
import Web.Scotty.Trans
import Web.Scotty.Internal.Types

isDebug :: IO Bool
isDebug =
  lookupEnv "DEBUG" <&> \case
    Just "1" -> True
    _ -> False

isHxRequest :: (MonadIO m) => ActionM m Bool
isHxRequest =
  header "hx-request" <&> \case
    Just "true" -> True
    _ -> False

formatLink :: (Monad m) => Inline -> m Inline
formatLink (Link (id_, cls, attrs) content target) = do
  return $ Link (id_, cls, ("rel", "noopener noreferrer") : ("target", "_blank") : attrs) content target
formatLink a = return a

formatFeedLink :: (Monad m) => Inline -> m Inline
formatFeedLink (Link (id_, cls, attr) content (url, title)) = do
  return (Link (id_, cls, attr) content (url, title))
formatFeedLink a = return a

mdToHtml :: (Pandoc -> PandocPure Pandoc) -> Text -> Html
mdToHtml w =
  fromRight mempty
    . runPure
    . ( writeHtml5
          def
            { writerExtensions =
                extensionsFromList
                  [ Ext_raw_html
                  , Ext_tex_math_dollars
                  ]
            , writerHTMLMathMethod = MathML
            }
          <=< w
          <=< readMarkdown
            def
              { readerExtensions =
                  extensionsFromList
                    [ Ext_backtick_code_blocks
                    , Ext_raw_html
                    , Ext_auto_identifiers
                    , Ext_tex_math_dollars
                    ]
              }
      )

scottySocketT' ::
  (MonadIO m) =>
  Maybe FilePath ->
  Options ->
  (AppConfigReaderM m Response -> IO Response) ->
  ScottyM m () ->
  IO ()
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

returnDefault :: a -> ScottyException -> ActionM IO a
returnDefault a = const (return a)

getPage :: ActionM IO Page
getPage = do
  pageLimit <- queryParam "limit" `catch` returnDefault 10
  pageOffset <- queryParam "offset" `catch` returnDefault 0

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
        H.! hx Get (fromString $ "/posts/" <> pid)
        H.! hx PushUrl "true"
        H.! hx Target "#posts"
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
            H.! hx Target "#posts"
            H.! hx Get (fromString . T.unpack $ "/posts/tags/" <> t)
            H.! hx PushUrl "true"
            $ H.toHtml t
    mdToHtml (walkM formatLink) $ postBody p

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

formatRfc822Gmt :: UTCTime -> String
formatRfc822Gmt = formatTime defaultTimeLocale "%a, %d %b %Y %R GMT"
