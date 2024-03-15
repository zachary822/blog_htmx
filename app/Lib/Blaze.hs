{-# LANGUAGE OverloadedStrings #-}

module Lib.Blaze where

import Control.Monad.IO.Class
import Data.String
import Lib.Types
import Text.Blaze.Html.Renderer.Utf8 (renderHtml)
import Text.Blaze.Html5 (Attribute, AttributeValue, Html)
import Text.Blaze.Html5 qualified as H
import Web.Scotty.Trans (raw, setHeader)

blazeHtml :: (MonadIO m) => Html -> ActionM m ()
blazeHtml h = do
  setHeader "Content-Type" "text/html; charset=utf-8"
  raw . renderHtml $ h

data HxAttribute
  = Get
  | Target
  | PushUrl
  deriving (Read)

instance Show HxAttribute where
  show Get = "get"
  show Target = "target"
  show PushUrl = "push-url"

hx :: HxAttribute -> AttributeValue -> Attribute
hx attr = H.customAttribute (fromString $ "hx-" <> show attr)
