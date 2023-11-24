{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskellQuotes #-}

module Lib.Blaze where

import Data.String
import Language.Haskell.TH.Quote
import Text.Blaze.Html.Renderer.Utf8 (renderHtml)
import Text.Blaze.Html5 (Attribute, AttributeValue, Html)
import Text.Blaze.Html5 qualified as H
import Text.Blaze.Html5.Attributes qualified as A
import Web.Scotty (ActionM, raw, setHeader)

classQQ :: QuasiQuoter
classQQ =
  QuasiQuoter
    { quoteExp = \str -> [|A.class_ . fromString . unwords . words $ str|]
    , quotePat = error "not supported"
    , quoteDec = error "not supported"
    , quoteType = error "not supported"
    }

blazeHtml :: Html -> ActionM ()
blazeHtml h = do
  setHeader "Content-Type" "text/html; charset=utf-8"
  raw . renderHtml $ h

hx :: String -> AttributeValue -> Attribute
hx attr = H.customAttribute (fromString $ "hx-" <> attr)
