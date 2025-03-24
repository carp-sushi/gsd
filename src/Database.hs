module Database
  ( createPool
  , createTestPool
  , runMigrations
  ) where

import Models (migrateAll)

import Control.Monad.Logger (runNoLoggingT)
import Data.String.Conversions (cs)
import Data.Text (Text)
import Database.Persist.Postgresql (createPostgresqlPool)
import Database.Persist.Sql (ConnectionPool, runMigrationSilent, runSqlPool)
import Database.Persist.Sqlite (createSqlitePool)

-- Create a database connection pool.
createPool :: Text -> Int -> IO ConnectionPool
createPool pgConn poolSize =
  runNoLoggingT $ do
    createPostgresqlPool (cs pgConn) poolSize

-- Create a sqlite database connection pool for testing.
-- TODO: Use test containers.
createTestPool :: Text -> Int -> IO ConnectionPool
createTestPool dbFile poolSize =
  runNoLoggingT $ do
    createSqlitePool (cs dbFile) poolSize

-- Run SQL migrations on a database.
runMigrations :: ConnectionPool -> IO ()
runMigrations pool =
  runNoLoggingT $ do
    _ <- runSqlPool (runMigrationSilent migrateAll) pool
    return ()
