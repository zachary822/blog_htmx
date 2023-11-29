{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Lib.Types where

import Control.Monad.IO.Unlift
import Control.Monad.Reader
import Data.Bifunctor
import Data.Pool
import Data.Text (Text)
import Data.Time.Calendar.Month
import Data.Time.Clock
import Database.MongoDB hiding (Oid)
import Database.MongoDB qualified as M
import GHC.Generics
import Katip qualified as K
import Web.Scotty.Trans

data AppConfig = AppConfig
  { getPool :: Pool Pipe
  , logNamespace :: K.Namespace
  , logContext :: K.LogContexts
  , logEnv :: K.LogEnv
  }

newtype App m a = App
  { unApp :: ReaderT AppConfig m a
  }
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader AppConfig, MonadUnliftIO)

instance (MonadIO m) => K.Katip (App m) where
  getLogEnv = asks logEnv
  localLogEnv f (App m) = App (local (\s -> s{logEnv = f (logEnv s)}) m)

instance (MonadIO m) => K.KatipContext (App m) where
  getKatipContext = asks logContext
  localKatipContext f (App m) = App (local (\s -> s{logContext = f (logContext s)}) m)
  getKatipNamespace = asks logNamespace
  localKatipNamespace f (App m) = App (local (\s -> s{logNamespace = f (logNamespace s)}) m)

type AppConfigReaderM m = App m
type AppConfigReader = AppConfigReaderM IO
type ScottyM = ScottyT AppConfigReader
type ActionM = ActionT AppConfigReader

logLocT :: K.Severity -> K.LogStr -> ActionM ()
logLocT = (lift .) . K.logLocM

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
