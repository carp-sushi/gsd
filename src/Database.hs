module Database
  ( createPool
  , runMigrations
  ) where

import Models (migrateAll)

import Control.Monad.Logger (runNoLoggingT)
import Data.String.Conversions (cs)
import Data.Text (Text)
import Database.Persist.Sqlite (ConnectionPool, createSqlitePool, runMigrationSilent, runSqlPool)

-- Create a database connection pool.
createPool :: Text -> Int -> IO ConnectionPool
createPool dbFile poolSize =
  runNoLoggingT $ do
    createSqlitePool (cs dbFile) poolSize

-- Run SQL migrations on a sqlite database.
runMigrations :: ConnectionPool -> IO ()
runMigrations pool =
  runNoLoggingT $ do
    _ <- runSqlPool (runMigrationSilent migrateAll) pool
    return ()
