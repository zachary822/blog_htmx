{-# LANGUAGE OverloadedStrings #-}

module Lib.Rss.Attributes (
  module Text.Blaze,
  module Lib.Rss.Attributes,
) where

import Text.Blaze
import Text.Blaze.Internal

href :: AttributeValue -> Attribute
href = attribute "href" "href=\""

rel :: AttributeValue -> Attribute
rel = attribute "rel" "rel=\""
