module Database
  ( createPool
  , runMigrations
  ) where

import Models (migrateAll)

import Control.Monad.Logger (runNoLoggingT, runStdoutLoggingT)
import Data.String.Conversions (cs)
import Data.Text (Text)
import Database.Persist.Postgresql (createPostgresqlPool)
import Database.Persist.Sql (ConnectionPool, runMigrationSilent, runSqlPool)

-- Create a database connection pool.
createPool :: Text -> Int -> IO ConnectionPool
createPool url size =
  runStdoutLoggingT $ do
    createPostgresqlPool (cs url) size

-- Run SQL migrations on a database.
runMigrations :: ConnectionPool -> IO ()
runMigrations pool =
  runNoLoggingT $ do
    _ <- runSqlPool (runMigrationSilent migrateAll) pool
    return ()
