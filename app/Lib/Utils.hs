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
import Data.Text (Text)
import Data.Time.Clock
import Data.Time.Format
import Database.MongoDB hiding (Oid, next)
import Database.MongoDB qualified as M
import Lib.Types
import Network.Socket as S
import Network.Wai (Response)
import System.Directory
import System.Environment
import System.Posix.Files
import Text.Hamlet
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
import Web.Scotty.Trans

isDebug :: IO Bool
isDebug = (Just "1" ==) <$> lookupEnv "DEBUG"

isHxRequest :: (MonadIO m) => ActionM m Bool
isHxRequest = (Just "true" ==) <$> header "hx-request"

formatLink :: (Monad m) => Inline -> m Inline
formatLink (Link (id_, cls, attrs) content target) = do
  return $
    Link
      (id_, cls, ("rel", "noopener noreferrer") : ("target", "_blank") : attrs)
      content
      target
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

getTotal :: Document -> Integer
getTotal = M.at "total"

getCurrentPage :: Page -> Integer
getCurrentPage page = pageOffset page `div` pageLimit page + 1

getMaxPage :: (Integral a, Integral b) => a -> Page -> b
getMaxPage total page = ceiling (fromIntegral total / fromIntegral (pageLimit page) :: Double)

formatRfc822Gmt :: UTCTime -> String
formatRfc822Gmt = formatTime defaultTimeLocale "%a, %d %b %Y %R GMT"
