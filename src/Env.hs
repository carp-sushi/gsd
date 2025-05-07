module Env
  ( Env (..),
    HandlerM,
  )
where

import Control.Monad.Trans.Reader (ReaderT)
import Database.Persist.Sql (ConnectionPool)
import Servant

-- Application environment.
newtype Env = Env
  { pool_ :: ConnectionPool
  }

-- Custom reader transformer monad for handlers.
type HandlerM = ReaderT Env Handler
