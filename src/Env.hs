module Env
  ( Env (..)
  , HandlerM
  )
where

import Config (Config)
import Control.Monad.Trans.Reader (ReaderT)
import Database.Persist.Sql (ConnectionPool)
import Servant

-- | App environment
data Env = Env
  { envConfig :: Config
  , envConnectionPool :: ConnectionPool
  }

-- | Custom reader transformer monad for handlers.
type HandlerM = ReaderT Env Handler
