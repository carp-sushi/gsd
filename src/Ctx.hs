module Ctx
  ( Ctx (..)
  , HandlerM
  ) where

import Control.Monad.Trans.Reader (ReaderT)
import Database.Persist.Sql (ConnectionPool)
import Servant

-- Application context.
newtype Ctx = Ctx
  { pool_ :: ConnectionPool
  }

-- Custom readwe monad for handlers.
type HandlerM = ReaderT Ctx Handler
