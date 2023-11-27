{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Lib.Middleware where

import Control.Monad
import Control.Monad.Trans.Maybe
import Data.ByteString (ByteString)
import Data.Function
import Data.List
import Data.Text qualified as T
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Network.HTTP.Types
import Network.URI
import Network.Wai
import Network.Wai.Middleware.Rewrite

isHxRequest :: Request -> Bool
isHxRequest = (Just "true" ==) . lookup "HX-Request" . requestHeaders

redirectNonHtmx :: ByteString -> Middleware
redirectNonHtmx r app req sendResponse = do
  let isHx = isHxRequest req
  if isHx
    then app req sendResponse
    else do
      sendResponse $
        responseLBS
          status302
          [ ("Location", r)
          ]
          ""

lookupT :: (Eq a, Monad m) => a -> [(a, b)] -> MaybeT m b
lookupT = (hoistMaybe .) . lookup

-- proper implementation
rewriteHtmxPosts :: PathsAndQueries -> RequestHeaders -> IO PathsAndQueries
rewriteHtmxPosts p@(ps, qs) h = do
  runMaybeT
    ( do
        lookupT "HX-Request" h >>= guard . (== "true")
        lookupT "HX-Trigger" h >>= guard . (== "posts")

        htmxUrl <- lookupT "HX-Current-Url" h
        htmxURI <- hoistMaybe $ parseURI . T.unpack $ decodeUtf8 htmxUrl

        let ps' = decodePathSegments . encodeUtf8 . T.pack $ uriPath htmxURI
        guard ((isPrefixOf `on` filter (not . T.null)) ps ps')

        let qs' = parseQuery . encodeUtf8 . T.pack $ uriQuery htmxURI

        return (ps', qs <> qs')
    )
    >>= \case
      Nothing -> return p
      Just p' -> return p'

-- scotty bug?
rewriteHtmxPostsMiddleware :: Middleware
rewriteHtmxPostsMiddleware app req sendResponse = do
  req' <- rewriteRequest rewriteHtmxPosts req
  app req'{rawQueryString = renderQuery True $ queryString req'} sendResponse
