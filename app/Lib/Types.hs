{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Lib.Types where

import Control.Monad.Trans.Reader
import Data.Bifunctor
import Data.Pool
import Data.Text (Text)
import Data.Time.Calendar.Month
import Data.Time.Clock
import Database.MongoDB hiding (Oid)
import Database.MongoDB qualified as M
import GHC.Generics
import Web.Scotty.Trans

newtype Config = Config
  { getPool :: Pool Pipe
  }

type ConfigReader = ReaderT Config IO
type ScottyM = ScottyT ConfigReader
type ActionM = ActionT ConfigReader

newtype Oid = Oid ObjectId

instance Parsable Oid where
  parseParam = second Oid . readEither

class FromDocument a where
  fromDocument :: Document -> a

instance FromDocument Document where
  fromDocument = id

data Post = Post
  { postId :: ObjectId
  , postTitle :: Text
  , postBody :: Text
  , postCreated :: UTCTime
  , postUpdated :: UTCTime
  , postTags :: [Text]
  }
  deriving (Generic, Show, Eq)

instance FromDocument Post where
  fromDocument d = Post{..}
   where
    postId = M.at "_id" d
    postTitle = M.at "title" d
    postBody = M.at "body" d
    postCreated = M.at "created" d
    postUpdated = M.at "updated" d
    postTags = M.at "tags" d

data MonthSummary = MonthSummary
  { summaryMonth :: Month
  , monthCount :: Integer
  }
  deriving (Generic, Show, Eq)

instance FromDocument MonthSummary where
  fromDocument d = MonthSummary{..}
   where
    summaryMonth = YearMonth (M.at "year" d) (M.at "month" d)
    monthCount = M.at "count" d

data TagSummary = TagSummary
  { tagName :: Text
  , tagCount :: Integer
  }
  deriving (Generic, Show, Eq)

instance FromDocument TagSummary where
  fromDocument d = TagSummary{..}
   where
    tagName = M.at "name" d
    tagCount = M.at "count" d

data Summary = Summary
  { monthly :: [MonthSummary]
  , tags :: [TagSummary]
  }
  deriving (Generic, Show, Eq)

instance FromDocument Summary where
  fromDocument d = Summary{..}
   where
    monthly = map fromDocument (M.at "monthly" d)
    tags = map fromDocument (M.at "tags" d)

data Page = Page
  { pageLimit :: Integer
  , pageOffset :: Integer
  }
  deriving (Show, Eq)
