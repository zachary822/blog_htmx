{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}

module Lib.Components where

import Data.List (intersperse)
import Data.Text (Text)
import Data.Time.Format
import Data.Time.Format.ISO8601
import Database.MongoDB hiding (Oid, next)
import Lib.Types
import Lib.Utils
import Text.Blaze.Html5 (ToMarkup)
import Text.Hamlet
import Text.Pandoc.Walk

timeEl :: (FormatTime t, ISO8601 t) => t -> Html
timeEl t =
  [shamlet|
$newline never
<time datetime=#{formatShow iso8601Format t}>#{formatTime defaultTimeLocale "%c" t}
|]

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
postHtml p =
  [shamlet|
<article class="prose">
  <a id=#{"posts:" <> pid} href=#{postUrl} hx-get=#{postUrl} hx-push-url="true" hx-target="#posts">
    <h1>#{postTitle p}
  <div class="mt-0 mb-1 text-sm">
    Created: ^{timeEl (postCreated p)} (Updated: ^{timeEl (postUpdated p)})
  <div class="my-0 flex gap-2">
    $forall t <- postTags p
      <button
        class="badge badge-neutral"
        hx-target="#posts"
        hx-get=#{"/posts/tags/" <> t}
        hx-push-url="true">#{t}
  ^{mdToHtml (walkM formatLink) (postBody p)}
|]
 where
  pid = show $ postId p
  postUrl = "/posts/" <> pid

divider :: Html
divider = [shamlet|<div class="divider">|]

renderPosts :: [Document] -> Html
renderPosts =
  sequence_
    . intersperse divider
    . map (postHtml . fromDocument)

pageButton :: (ToMarkup a, ToMarkup b) => Bool -> a -> b -> Html
pageButton active url i =
  [shamlet|
<a
  class=#{class_}
  hx-push-url="true"
  hx-target="#posts"
  href=#{url}
  hx-get=#{url}>#{i}
|]
 where
  class_ :: Text = if active then "join-item btn btn-active" else "join-item btn"
