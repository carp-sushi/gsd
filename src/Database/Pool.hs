module Database.Pool where

import Control.Monad.Logger (runStdoutLoggingT)
import Data.String.Conversions (cs)
import Data.Text (Text)
import Database.Persist.Postgresql (ConnectionPool, createPostgresqlPool)

-- Create a database connection pool.
createPool :: Text -> Int -> IO ConnectionPool
createPool url size =
  runStdoutLoggingT $ do
    createPostgresqlPool (cs url) size
