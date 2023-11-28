module Lib.Types where

import Control.Monad.Trans.Reader
import Data.Pool
import Database.MongoDB
import Web.Scotty.Trans

newtype Config = Config
  { getPool :: Pool Pipe
  }

type ConfigReader = ReaderT Config IO
type ScottyM = ScottyT ConfigReader
type ActionM = ActionT ConfigReader
