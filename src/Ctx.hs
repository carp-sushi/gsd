module Ctx
  ( Ctx (..)
  ) where

import Database.Persist.Sql (ConnectionPool)

-- Application context.
newtype Ctx = Ctx
  { pool_ :: ConnectionPool
  }
