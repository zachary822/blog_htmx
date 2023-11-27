{-# LANGUAGE OverloadedStrings #-}

module Lib.Rss where

import Data.String
import Text.Blaze
import Text.Blaze.Internal (MarkupM (..))
import Text.Blaze.Renderer.Utf8 (renderMarkup)
import Web.Scotty (ActionM, raw, setHeader)

type RSS = Markup

rssXml :: RSS -> ActionM ()
rssXml r = do
  setHeader "Content-Type" "application/rss+xml"
  raw . renderMarkup $ r

xmlHeader :: RSS
xmlHeader = preEscapedText "<?xml version=\"1.0\" encoding=\"UTF-8\"?>"

version :: AttributeValue -> Attribute
version = customAttribute "version"

xmlns :: String -> AttributeValue -> Attribute
xmlns ns = customAttribute (fromString $ "xmlns:" <> ns)

rss :: RSS -> RSS
rss = Parent "rss" "<rss" "</rss>" ! version "2.0" ! xmlns "atom" "http://www.w3.org/2005/Atom"

channel :: RSS -> RSS
channel = Parent "channel" "<channel" "</channel>"

title :: RSS -> RSS
title = Parent "title" "<title" "</title>"

link :: RSS -> RSS
link = Parent "link" "<link" "</link>"

description :: RSS -> RSS
description = Parent "description" "<description" "</description>"

language :: RSS -> RSS
language = Parent "language" "<language" "</language>"

pubDate :: RSS -> RSS
pubDate = Parent "pubDate" "<pubDate" "</pubDate>"

atomLink :: RSS
atomLink = Leaf "atom:link" "<atom:link" ">" ()

item :: RSS -> RSS
item = Parent "item" "<item" "</item>"
