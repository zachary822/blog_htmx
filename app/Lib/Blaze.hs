{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskellQuotes #-}

module Lib.Blaze where

import Control.Monad.IO.Class
import Data.String
import Language.Haskell.TH.Quote
import Lib.Types
import Text.Blaze.Html.Renderer.Utf8 (renderHtml)
import Text.Blaze.Html5 (Attribute, AttributeValue, Html)
import Text.Blaze.Html5 qualified as H
import Text.Blaze.Html5.Attributes qualified as A
import Web.Scotty.Trans (raw, setHeader)

classQQ :: QuasiQuoter
classQQ =
  QuasiQuoter
    { quoteExp = \str -> [|A.class_ . fromString . unwords . words $ str|]
    , quotePat = error "not supported"
    , quoteDec = error "not supported"
    , quoteType = error "not supported"
    }

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
