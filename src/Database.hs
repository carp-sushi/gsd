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
createPool url size =
  runNoLoggingT $ do
    createPostgresqlPool (cs url) size

-- Create a sqlite database connection pool for testing.
createTestPool :: Text -> Int -> IO ConnectionPool
createTestPool file size =
  runNoLoggingT $ do
    createSqlitePool (cs file) size

-- Run SQL migrations on a database.
runMigrations :: ConnectionPool -> IO ()
runMigrations pool =
  runNoLoggingT $ do
    _ <- runSqlPool (runMigrationSilent migrateAll) pool
    return ()
